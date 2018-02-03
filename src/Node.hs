{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Node
    ( establishClusterConnection
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor
import Debug.Trace
import Block
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Crypto.Sign.Ed25519 (PublicKey(..), Signature, unSignature)
import Data.Binary
import Data.ByteString (ByteString)
import Data.ByteString.Base58
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map.Strict as Map
import Data.List as List
import Data.List.NonEmpty( NonEmpty( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup
import Exchange
import qualified GHC.Generics as G
import Ledger
import NodeCommandLine
import Serokell.Communication.IPC
import Utils
import Transaction
import Text.PrettyPrint.Boxes as Boxes (render, vcat, hsep, left, text)

data NodeState = NodeState
    { nodeConfig :: NodeConfig
    , neighbours :: [NodeId]
    , blockchain :: MVar BlockChain
    , transactionPool :: MVar [Transaction]
    , nodeLedger :: MVar Ledger
    , broadcastChannel :: Chan Broadcast
    }

calculateNeighbours :: NodeConfig -> [NodeId]
calculateNeighbours nodeConfig =
    let nId = (unNodeId . nodeId) nodeConfig
    in NodeId <$> filter (\a -> a /= nId) [0 .. nodeCount nodeConfig]

initialNodeState :: NodeConfig -> IO NodeState
initialNodeState nodeConfig = do
    emptyBlockChain <- newMVar $ genesisBlock :| []
    emptyTransactionPool <- newMVar []
    ledger <- newMVar (Ledger genesisLedger)
    chan <- newChan
    pure $
        NodeState
            nodeConfig
            (calculateNeighbours nodeConfig)
            emptyBlockChain
            emptyTransactionPool
            ledger
            chan

mineblock :: NodeState -> IO ()
mineblock nodeState = loop where

    loop = do
      tId <- myThreadId
      threadDelay $ 15 * 1000 * 1000
      txs <- readMVar $ transactionPool nodeState
      if null txs then putStrLn $ "[" <> show tId <> "] No Transaction to mine." else mine txs
      loop

    mine :: [Transaction] -> IO ()
    mine txs = do
      tId <- myThreadId
      chain <- readMVar $ blockchain nodeState
      timestamp <- now
      let lastBlock = NEL.head chain
      let nextBlockId = 1 + index lastBlock
      let nextBlock = Block nextBlockId txs timestamp
      maybeValidBlock <- addBlock nextBlock nodeState
      case maybeValidBlock of
        Nothing -> putStrLn $ "[" <> show tId <> "] Mining failed, block number " <> show nextBlock <> " already exists."
        (Just validBlock) -> do
          writeChan (broadcastChannel nodeState) (BlockBroadcast validBlock)
          let validCount = (length . transactions) validBlock
          let invalidCount = length txs - validCount
          putStrLn $ "[" <> show tId <> "] Mined block " <> show nextBlockId <> " of " <> show validCount <> " valid transactions and " <> show invalidCount <> " invalid transactions."

transactionInBlockchain :: Transaction -> BlockChain -> Bool
transactionInBlockchain transaction blocks =
  or $ (any (== transaction)) <$> transactions <$> blocks

handleNodeExchange :: NodeState -> NodeExchange -> IO (ExchangeResponse, StateAction)
handleNodeExchange nodeState msg =
    case msg of
        AddTransaction tx -> do
            tId <- myThreadId
            bchain <- readMVar $ blockchain nodeState
            txs <- readMVar $ transactionPool nodeState
            let hasTx = any (\t -> t == tx) txs
            if hasTx || transactionInBlockchain tx bchain
                then do putStrLn $ "[" <> show tId <> "] Transaction already exists: " <> show tx
                        pure $ (NExchangeResp 1, NoAction)
                else do
                    modifyMVar_
                        (transactionPool nodeState)
                        (\txs -> pure $ tx : txs)
                    putStrLn $ "[" <> show tId <> "] Transaction " <> show tx <> " successsfully added to the poll and to the broadcast. Total number of transactions in pool: " <> (show $ length txs + 1)
                    writeChan (broadcastChannel nodeState) (TxBroadcast tx)
                    pure $ (NExchangeResp 1, NoAction)
        QueryBlock n -> do
            blocks <- readMVar (blockchain nodeState)
            let block = find (\b -> Block.index b == n) blocks
            case block of
                Nothing -> pure $ (BlockResponse Nothing, NoAction)
                (Just b) -> pure $ (BlockResponse (Just b), NoAction)
        AddBlock block -> do
          addBlock block nodeState
          pure $ (NExchangeResp 1, NoAction)


addBlock :: Block -> NodeState -> IO (Maybe Block)
addBlock block nodeState = do
  blocks <- takeMVar (blockchain nodeState)
  let recievedBlockId = index block
  let expectedBlockId = ((+1) . index . NEL.head) blocks
  if expectedBlockId == recievedBlockId
    then do
      tId <- myThreadId

      putStrLn $ "[" <> show tId <> "] Adding block " <> show recievedBlockId <> " to the blockchain."
      -- Remove transaction from transaction pool which are in the newly added block
      let txs = transactions block
      modifyMVar_ (transactionPool nodeState) (\txs2 -> pure $ txs2 \\ txs)

      -- Update Ledger
      putStrLn $ "[" <> show tId <> "] Reconciling ledger!"
      let ledgerM = nodeLedger nodeState
      ledger <- takeMVar ledgerM
      let (errors, ledgerUpd, validTransactions) = applyTransactions ledger txs

      let validBlock = block { transactions = validTransactions }

      reportProblems errors

      putMVar ledgerM ledgerUpd

      -- Add block itself
      putMVar (blockchain nodeState) (validBlock <| blocks)
      pure $ Just validBlock
    else do
      tId <- myThreadId
      putMVar (blockchain nodeState) blocks
      putStrLn $ "[" <> show tId <> "] Expecting " <> show expectedBlockId <> " block index. Received " <>  show recievedBlockId <> " block index."
      pure Nothing

reportProblems :: [LedgerError] -> IO ()
reportProblems errors = do
  tId <- myThreadId
  sequence_ $ (putStrLn . (("[" <> show tId <> "] ") <>) . show) <$> errors

applyTransactions :: Ledger -> [Transaction] -> ([LedgerError], Ledger, [Transaction])
applyTransactions ledger = foldr applyTransaction ([], ledger, [])

applyTransaction :: Transaction -> ([LedgerError], Ledger, [Transaction]) ->  ([LedgerError], Ledger, [Transaction])
applyTransaction tx (errors, l@(Ledger ledger), validTransactions) =
  case res of
    Left error -> (error : errors, l, validTransactions)
    Right led -> (errors, led, tx : validTransactions)
  where
    tran = transfer tx
    fromAccount = (Address . encodePublicKey . from) tran
    toAccount = to tran
    amountToPay = amount tran

    res = do
      f <- balance l fromAccount
      if f >= amountToPay
        then let ledgerUpd = Map.insertWith (flip (-)) fromAccount amountToPay ledger
                 ledgerUpd2 = Map.insertWith (+) toAccount amountToPay ledgerUpd
             in Right (Ledger ledgerUpd2)
        else Left $ InsufficientBalance fromAccount toAccount amountToPay

balance :: Ledger -> Address -> Either LedgerError Balance
balance (Ledger ledger) address =
    maybe (Left $ AddressNotFound address) Right (Map.lookup address ledger)

toExchange :: ByteString -> Exchange
toExchange = decode . BL.fromStrict

toExchangeResponse :: ByteString -> ExchangeResponse
toExchangeResponse = decode . BL.fromStrict

deriveAddress :: PublicKey -> Address
deriveAddress = Address . encodePublicKey

txInfo :: Transaction -> [String]
txInfo tx = let
  genesisPk = (BS.unpack . rawAddress . deriveAddress . from . transfer) tx
  to2 = (BS.unpack . rawAddress . to . transfer) tx
  amount2 = (show . amount . transfer) tx
  in [genesisPk, to2, amount2]

txsInfo :: [Transaction] -> [[String]]
txsInfo txs = txInfo <$> txs

blockInfo :: Block -> String
blockInfo block = let
  blockIndex = (show . index) block
  tsBlock = Block.timestamp block
  txs = (txsInfo . transactions) block
  txsWithBlockId = txs ++ [[blockIndex]]
  txsBox = render $ hsep 2 left (map (vcat left . map Boxes.text) (transpose txsWithBlockId))
  in txsBox

nodeStatus :: NodeState -> IO NodeInfo
nodeStatus nodeState = do
    txSize <- readMVar (transactionPool nodeState)
    blockChain <- readMVar (blockchain nodeState)
    ledger <- readMVar (nodeLedger nodeState)
    let blocksInfo = blockInfo <$> blockChain
    pure $
        NodeInfo
            (unNodeId ((nodeId . nodeConfig) nodeState))
            (length txSize)
            (length blockChain)
            (unNodeId <$> neighbours nodeState)
            blocksInfo
            (show ledger)

handleClientNodeExchange :: NodeState
                         -> ClientNodeExchange
                         -> IO (ExchangeResponse, StateAction)
handleClientNodeExchange nodeState clientNodeExchange =
    case clientNodeExchange of
        MakeTransfer transfer signature -> do
            case verifyTransfer signature transfer of
                True -> do
                    tId <- myThreadId
                    putStrLn $  "[" <> show tId <> "] Signature verified " <> show transfer
                    timestamp <- now
                    let transactionId = TransactionId $  hashSignature signature
                    let tx = Transaction transactionId transfer signature timestamp
                    pure $ (SubmitResp $ Just transactionId, AddTransactionToNode tx)
                False -> pure $ (SubmitResp Nothing, NoAction)

        AskBalance address -> do
          Ledger ledger <- readMVar (nodeLedger nodeState)
          pure $ case Map.lookup address ledger of
            Nothing -> (StringResp $ "Balance error, unknown address: " <> show address, NoAction)
            Just b -> (BalanceResp b, NoAction)

        Register address -> do
                    (Ledger ledger) <- readMVar $ nodeLedger nodeState
                    txs <- readMVar $ transactionPool nodeState
                    let addressStr = (BS.unpack $ rawAddress address)
                    if Map.member address ledger || any (\tx -> (to . transfer) tx == address) txs
                      then pure $ (StringResp $ "Address " <> addressStr <> " already registered", NoAction)
                      else do
                        let genesisPk = (from . fst) genesisTransfer
                        let transfer = Transfer genesisPk address 1000
                        let signature = signTransfer (snd nodeKeyPair) transfer
                        let transactionId = TransactionId $ hashSignature signature
                        timestamp <- now
                        let tx = Transaction transactionId transfer signature timestamp
                        pure $ (StringResp $  "Registration successful: " <> addressStr, AddTransactionToNode tx)

        FetchStatus -> do
            nodeInfo <- nodeStatus nodeState
            pure $ (StatusInfo nodeInfo, NoAction)

broadcastToExchange :: Broadcast -> NodeExchange
broadcastToExchange (TxBroadcast tx) = AddTransaction tx
broadcastToExchange (BlockBroadcast block) = AddBlock block

synchonizeBlockChain :: NodeState -> Int -> Conversation -> IO ()
synchonizeBlockChain nodeState n cc @ Conversation {..} = do
  tId <- myThreadId
  putStrLn $ "[" <> show tId <> "] Synchronizing. Asking for block " <> show n
  response <- send (BL.toStrict $ encode (NExchange $ QueryBlock n)) *> recv
  case toExchangeResponse response of
    BlockResponse (Just block) -> do
      putStrLn $ "[" <> show tId <> "] Synchronizing. Block number " <> show n <> " received. Adding it to blockchain"
      addBlock block nodeState
      synchonizeBlockChain nodeState (n + 1) cc
    BlockResponse Nothing -> putStrLn $ "[" <> show tId <> "] Synchronizing. Block number " <> show n <> " don't received. Synchronization complete."
    other -> putStrLn $  "[" <> show tId <> "] Error: Expected BlockResponse got: " <> show other

neighbourHandler :: NodeState -> Chan Broadcast -> NodeId -> Conversation -> IO ()
neighbourHandler nodeState broadcastChannel nodeId cc @ Conversation {..} = do
  blocks <- readMVar (blockchain nodeState)
  synchonizeBlockChain nodeState (List.length blocks + 1) cc
  loop where
  loop = do
    tId <- myThreadId
    putStrLn $
        "[" <> show tId <> "] CONNECTED to nodeId " <> show (unNodeId nodeId) <>
        ", reading from channel"
    broadcast <- readChan broadcastChannel
    response <- send (BL.toStrict $ encode (NExchange $ broadcastToExchange broadcast)) *> recv
    let dec = toExchangeResponse response
    putStrLn $ "[" <> show tId <> "] CONNECTED and recieved nodeId " <> show (unNodeId nodeId) <> ", exchange response " <> show dec
    loop --neighbourHandler nodeState broadcastChannel nodeId cc

connectToNeighbour :: NodeState -> Chan Broadcast -> NodeId -> IO ()
connectToNeighbour nodeState broadcastChan nodeId =
    try
        (connectToUnixSocket
             "sockets"
             nodeId
             (neighbourHandler nodeState broadcastChan nodeId)) >>=
    retry nodeState broadcastChan nodeId

retry :: NodeState -> Chan Broadcast -> NodeId -> Either IOException a -> IO ()
retry nodeState broadcastChan nodeId (Right aa) = do
    tId <- myThreadId
    putStrLn $ "[" <> show tId <> "] OK connection to " <> show (unNodeId nodeId) <> " successful!!!"
retry nodeState broadcastChan nodeId (Left ex) = do
    tId <- myThreadId
    putStrLn $ "[" <> show tId <> "] Connection to nodeId : " <> show (unNodeId nodeId) <> " failed: " <>
         show ex
    putStrLn $ "[" <> show tId <> "] Attempting to reconnect in 10s"
    (threadDelay $ 10 * 1000 * 1000)
    putStrLn $ "[" <> show tId <> "] Trying reestablish connection with nodeId " <> show (unNodeId nodeId)
    connectToNeighbour nodeState broadcastChan nodeId

nodeIdFromState :: NodeState -> NodeId
nodeIdFromState = nodeId . nodeConfig

nodeIdFromStateStr :: NodeState -> String
nodeIdFromStateStr = show . unNodeId . nodeIdFromState

commu :: NodeState -> Conversation -> IO Bool
commu nodeState conversation = do
    True <$ do
        forkIO
            ((do tId <- myThreadId
                 (putStrLn $
                  "[" <> show tId <> "] node " <> myId <> ". Forking new thread!")) <* do
                 loopO nodeState)
  where
    myId = nodeIdFromStateStr nodeState
    loopO :: NodeState -> IO ()
    loopO nodeState = loop
      where
        loop :: IO ()
        loop = do
            input <- recv conversation
            tId <- myThreadId
            putStrLn $
                "[" <> show tId <> "] node " <> myId <>
                " received some input " <>
                (show (length (BS.unpack input)))
            let exchange = toExchange input
            putStrLn $ "[" <> show tId <> "] received exchange: " <> show exchange
            (exchangeResponse, action) <-
                case exchange of
                    (NExchange nodeExchange) ->
                        handleNodeExchange nodeState nodeExchange
                    (CExchange clientNodeExchange) ->
                        handleClientNodeExchange nodeState clientNodeExchange
            case action of
                NoAction -> pure ()
                (AddTransactionToNode tx) -> do
                    handleNodeExchange nodeState (AddTransaction tx)
                    pure ()
            send conversation (BL.toStrict (encode exchangeResponse))
            nextStep (BS.unpack input) loop

nextStep :: String -> IO () -> IO ()
nextStep "" io = putStrLn "Closed by peer!"
nextStep _ io = io

startMinerThread :: NodeState -> IO ()
startMinerThread nodeState = void $ forkIO $ do
  mineblock nodeState

establishClusterConnection :: NodeConfig -> IO ()
establishClusterConnection nodeConfig = do
    nodeState <- initialNodeState nodeConfig
    startMinerThread nodeState
    connectToNeighbours nodeState
    listenForClientConnection nodeState

logMessage :: NodeId -> NodeId -> String -> IO ()
logMessage nodeId neighbourId msg =
    let myId = show (unNodeId nodeId)
    in do
      tId <- myThreadId
      putStrLn $ "[" <> show tId <> "] node " <> myId <> ". " <> msg <> " with node: " <>
            show (unNodeId neighbourId)

connectToNeighbours :: NodeState -> IO [ThreadId]
connectToNeighbours nodeState =
    let nodeNeighbours = (neighbours nodeState)
        myId = nodeIdFromState nodeState
    in do sequence $
              (\nId ->
                   forkIO
                       (do logMessage myId nId "Trying to establish connecting"
                           myChan <- dupChan (broadcastChannel nodeState)
                           connectToNeighbour nodeState myChan nId
                           logMessage myId nId "Terminating connection")) <$>
              nodeNeighbours

listenForClientConnection :: NodeState -> IO ()
listenForClientConnection nodeState = do
    listenUnixSocket "sockets" (nodeIdFromState nodeState) (commu nodeState)
