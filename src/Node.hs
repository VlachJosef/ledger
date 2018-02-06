{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Node
    ( establishClusterConnection
    ) where

import Address
import Data.Functor
import Block
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Crypto.Sign.Ed25519 (PublicKey(..), Signature, unSignature)
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.List as List
import Data.List.NonEmpty( NonEmpty( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup
import Exchange
import Ledger
import NodeCommandLine
import Serokell.Communication.IPC
import Utils
import Transaction
import Text.PrettyPrint.Boxes as Boxes (render, vcat, hsep, left, text)
import Time

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
      threadDelay $ 15 * 1000 * 1000
      txs <- readMVar $ transactionPool nodeState
      if null txs then logThread $ "No Transaction to mine." else mine txs
      loop

    mine :: [Transaction] -> IO ()
    mine txs = do
      chain <- readMVar $ blockchain nodeState
      timestamp <- now
      let lastBlock = NEL.head chain
      let nextBlockId = 1 + index lastBlock
      let nextBlock = Block nextBlockId txs timestamp
      maybeValidBlock <- addBlock nextBlock nodeState
      case maybeValidBlock of
        Nothing -> logThread $ "Mining failed, block number " <> show nextBlock <> " already exists."
        Just validBlock -> do
          writeChan (broadcastChannel nodeState) (BlockBroadcast validBlock)
          let validCount = (length . transactions) validBlock
          let invalidCount = length txs - validCount
          logThread $ "Mined block " <> show nextBlockId <> " of " <> show validCount <> " valid transactions and " <> show invalidCount <> " invalid transactions."

transactionInBlockchain :: Transaction -> BlockChain -> Bool
transactionInBlockchain transaction blocks =
  or $ (any (== transaction)) <$> transactions <$> blocks

transactionIdInBlockchain :: TransactionId -> BlockChain -> Bool
transactionIdInBlockchain txId blocks =
  or $ (any (== txId)) <$> (map transactionId) <$> transactions <$> blocks

handleNodeExchange :: NodeState -> NodeExchange -> IO NodeExchangeResponse
handleNodeExchange nodeState =
  \case
    AddTransaction tx -> do
      blocks <- readMVar $ blockchain nodeState
      txs <- readMVar $ transactionPool nodeState
      let hasTx = any (\t -> t == tx) txs
      if hasTx || transactionInBlockchain tx blocks
        then do logThread $ "Transaction already exists: " <> show tx
                pure NodeNoResponse
        else do
            modifyMVar_
                (transactionPool nodeState)
                (\txs -> pure $ tx : txs)
            logThread $ "Transaction " <> show tx <> " successsfully added to the poll and to the broadcast. Total number of transactions in pool: " <> (show $ length txs + 1)
            writeChan (broadcastChannel nodeState) (TxBroadcast tx)
            pure NodeNoResponse
    QueryBlock n -> do
      blocks <- readMVar (blockchain nodeState)
      let block = find (\b -> Block.index b == n) blocks
      pure $ BlockResponse block
    AddBlock block -> do
      addBlock block nodeState
      pure NodeNoResponse


addBlock :: Block -> NodeState -> IO (Maybe Block)
addBlock block nodeState = do
  blocks <- takeMVar (blockchain nodeState)
  let recievedBlockId = index block
  let expectedBlockId = ((+1) . index . NEL.head) blocks
  if expectedBlockId == recievedBlockId
    then do
      logThread $ "Adding block " <> show recievedBlockId <> " to the blockchain."
      -- Remove transactions from transaction pool which are in the newly added block
      let txs = transactions block
      modifyMVar_ (transactionPool nodeState) (\txs2 -> pure $ txs2 \\ txs)

      -- Update Ledger
      logThread $ "Reconciling ledger!"
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
      putMVar (blockchain nodeState) blocks
      logThread $ "Expecting " <> show expectedBlockId <> " block index. Received " <>  show recievedBlockId <> " block index."
      pure Nothing

reportProblems :: [LedgerError] -> IO ()
reportProblems errors = sequence_ $ (logThread . show) <$> errors

applyTransactions :: Ledger -> [Transaction] -> ([LedgerError], Ledger, [Transaction])
applyTransactions ledger = foldr applyTransaction ([], ledger, [])

applyTransaction :: Transaction -> ([LedgerError], Ledger, [Transaction]) -> ([LedgerError], Ledger, [Transaction])
applyTransaction tx (errors, l@(Ledger ledger), validTransactions) =
  case updatedLedger of
    Left error -> (error : errors, l, validTransactions)
    Right led -> (errors, led, tx : validTransactions)
  where
    tran = transfer tx
    fromAccount = (deriveAddress . from) tran
    toAccount = to tran
    amountToPay = amount tran

    updatedLedger = do
      f <- balance l fromAccount
      if f >= amountToPay
        then let ledgerUpd = Map.insertWith (flip (-)) fromAccount amountToPay ledger
                 ledgerUpd2 = Map.insertWith (+) toAccount amountToPay ledgerUpd
             in Right $ Ledger ledgerUpd2
        else Left $ InsufficientBalance fromAccount toAccount amountToPay

balance :: Ledger -> Address -> Either LedgerError Balance
balance (Ledger ledger) address =
    maybe (Left $ AddressNotFound address) Right (Map.lookup address ledger)

txInfo :: Transaction -> [String]
txInfo tx =
  ($tx) <$> [ show . deriveAddress . from . transfer
            , show . to . transfer
            , show . amount . transfer]

txsInfo :: [Transaction] -> [[String]]
txsInfo txs = txInfo <$> txs

blockInfo :: Block -> String
blockInfo block = let
  blockIndex = (show . index) block
  txs = (txsInfo . transactions) block
  txsWithBlockId = txs ++ [[blockIndex]]
  in render $ hsep 2 left (map (vcat left . map Boxes.text) (transpose txsWithBlockId))

nodeStatus :: NodeState -> IO NodeInfo
nodeStatus nodeState = do
    txSize <- readMVar (transactionPool nodeState)
    blocks <- readMVar (blockchain nodeState)
    ledger <- readMVar (nodeLedger nodeState)
    let blocksInfo = blockInfo <$> blocks
    pure $
        NodeInfo
            ((unNodeId . nodeId . nodeConfig) nodeState)
            (length txSize)
            (length blocks)
            (unNodeId <$> neighbours nodeState)
            blocksInfo
            (show ledger)

createTransaction :: Signature -> TransactionId
createTransaction = TransactionId . encodeSignature 

handleClientExchange :: NodeState
                         -> ClientExchange
                         -> IO ClientExchangeResponse
handleClientExchange nodeState =
  \case
    MakeTransfer transfer signature ->
      if verifyTransfer signature transfer
        then do
          logThread $ "Signature verified " <> show transfer
          timestamp <- now
          let transactionId = createTransaction signature
          let tx = Transaction transactionId transfer signature timestamp
          handleNodeExchange nodeState (AddTransaction tx)
          pure $ SubmitResp $ Just transactionId
        else pure $ SubmitResp Nothing

    AskBalance address -> do
      Ledger ledger <- readMVar (nodeLedger nodeState)
      pure $ case Map.lookup address ledger of
        Nothing -> StringResp $ "Balance error, unknown address: " <> show address
        Just balance -> BalanceResp balance

    Query txId -> do
      blocks <- readMVar (blockchain nodeState)
      let wasAdded = transactionIdInBlockchain txId blocks
      pure $ QueryResp wasAdded

    Register address -> do
      ledger <- readMVar $ nodeLedger nodeState
      txs <- readMVar $ transactionPool nodeState
      timestamp <- now
      let maybeTx = isClientRegistered address ledger txs timestamp
      case maybeTx of
        Just tx -> do
          handleNodeExchange nodeState (AddTransaction tx)
          pure $ StringResp $ "Registration successful: " <> show address
        Nothing -> pure $ StringResp $ "Address " <> show address <> " already registered."

    FetchStatus -> do
      nodeInfo <- nodeStatus nodeState
      pure $ StatusInfo nodeInfo

isClientRegistered :: Address -> Ledger -> [Transaction] -> Timestamp -> Maybe Transaction
isClientRegistered address (Ledger ledger) txs timestamp =
   if Map.member address ledger || any (\tx -> (to . transfer) tx == address) txs
     then Nothing
     else let genesisPk = (from . fst) genesisTransfer
              transfer = Transfer genesisPk address 1000
              signature = signTransfer (snd nodeKeyPair) transfer
              transactionId = createTransaction signature
              in Just $ Transaction transactionId transfer signature timestamp

broadcastToExchange :: Broadcast -> NodeExchange
broadcastToExchange = \case
  TxBroadcast tx -> AddTransaction tx
  BlockBroadcast block -> AddBlock block

synchonizeBlockChain :: NodeState -> Int -> Conversation -> IO ()
synchonizeBlockChain nodeState n cc @ Conversation {..} = do
  logThread $ "Synchronizing. Asking for block " <> show n
  response <- send (encodeNodeExchange $ QueryBlock n) *> recv
  case decodeNodeExchangeResponse response of
    BlockResponse (Just block) -> do
      logThread $ "Synchronizing. Block number " <> show n <> " received. Adding it to blockchain"
      addBlock block nodeState
      synchonizeBlockChain nodeState (n + 1) cc
    BlockResponse Nothing -> logThread $ "Synchronizing. Block number " <> show n <> " don't received. Synchronization complete."
    other -> logThread $ "Error: Expected BlockResponse got: " <> show other

neighbourHandler :: NodeState -> Chan Broadcast -> NodeId -> Conversation -> IO ()
neighbourHandler nodeState broadcastChannel nodeId cc @ Conversation {..} = do
  blocks <- readMVar (blockchain nodeState)
  synchonizeBlockChain nodeState (List.length blocks + 1) cc
  loop where
  loop = do
    logThread $ "Connected to nodeId " <> show nodeId <> ", reading from channel"
    broadcast <- readChan broadcastChannel
    response <- send (encodeNodeExchange $ broadcastToExchange broadcast) *> recv
    let dec = decodeNodeExchangeResponse response
    logThread $ "Connected and recieved nodeId " <> show nodeId <> ", exchange response " <> show dec
    loop

encodeNodeExchange :: NodeExchange -> ByteString
encodeNodeExchange = BL.toStrict . encode . NodeExchange 

connectToNeighbour :: NodeState -> Chan Broadcast -> NodeId -> IO ()
connectToNeighbour nodeState broadcastChan nodeId =
    try
        (connectToUnixSocket
             "sockets"
             nodeId
             (neighbourHandler nodeState broadcastChan nodeId)) >>=
    retry nodeState broadcastChan nodeId

logThread :: String -> IO ()
logThread msg = do
    tId <- myThreadId
    putStrLn $ "[" <> show tId <> "] " <> msg

retry :: NodeState -> Chan Broadcast -> NodeId -> Either IOException a -> IO ()
retry nodeState broadcastChan nodeId = let
  nId = show (unNodeId nodeId)
  in \case
  Right _ ->
    logThread $ "Connection to nodeId " <> nId <> " successful!!!"
  Left ex -> do
    logThread $ "Connection to nodeId " <> nId <> " failed: " <> show ex
    logThread $ "Will attempt to reconnect in 10s."
    threadDelay $ 10 * 1000 * 1000
    logThread $ "Trying reestablish connection with nodeId " <> nId
    connectToNeighbour nodeState broadcastChan nodeId

nodeIdFromState :: NodeState -> NodeId
nodeIdFromState = nodeId . nodeConfig

nodeIdFromStateStr :: NodeState -> String
nodeIdFromStateStr = show . unNodeId . nodeIdFromState

commu :: NodeState -> Conversation -> IO Bool
commu nodeState conversation = do
    True <$ do
        forkIO $
            logThread ("node " <> myId <> ". Forking new thread!") <* loopO nodeState
  where
    myId = nodeIdFromStateStr nodeState
    loopO :: NodeState -> IO ()
    loopO nodeState = loop
      where
        loop :: IO ()
        loop = do
            input <- recv conversation
            let exchange = decodeExchange input
            logThread $ "node " <> myId <> " received " <> show exchange
            exchangeResponse <-
                case exchange of
                    NodeExchange nodeExchange ->
                       encode <$> handleNodeExchange nodeState nodeExchange
                    ClientExchange clientNodeExchange ->
                       encode <$> handleClientExchange nodeState clientNodeExchange

            send conversation (BL.toStrict exchangeResponse)
            nextStep (BS.unpack input) loop

nextStep :: String -> IO () -> IO ()
nextStep "" io = logThread "Closed by peer!"
nextStep _ io = io

startMinerThread :: NodeState -> IO ()
startMinerThread = void . forkIO . mineblock 

establishClusterConnection :: NodeConfig -> IO ()
establishClusterConnection nodeConfig = do
    nodeState <- initialNodeState nodeConfig
    startMinerThread nodeState
    connectToNeighbours nodeState
    listenForClientConnection nodeState

logMessage :: NodeId -> NodeId -> String -> IO ()
logMessage nodeId neighbourId msg =
   let myId = show (unNodeId nodeId)
       nId = show (unNodeId neighbourId)
   in logThread $ "node " <> myId <> ". " <> msg <> " with node: " <> nId

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
