{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Node
    ( establishClusterConnection
    ) where

import Data.Functor
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
import Data.List
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
    ledger <- newMVar emptyLedger
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
      if null txs then putStrLn "No Transaction to mine." else mine txs
      loop

    mine :: [Transaction] -> IO ()
    mine txs = do
      chain <- readMVar $ blockchain nodeState
      timestamp <- now
      let lastBlock = NEL.head chain
      let nextBlockId = 1 + index lastBlock
      let nextBlock = Block nextBlockId txs timestamp
      modifyMVar_ (transactionPool nodeState) (\txs2 -> pure $ txs2 \\ txs)
      modifyMVar_ (blockchain nodeState) (\blocks -> pure $ nextBlock <| blocks)
      writeChan (broadcastChannel nodeState) (BlockBroadcast nextBlock)
      putStrLn $ "Mined block " <> show nextBlockId <> " of " <> show (length txs) <> " transactions"


handleNodeMsg :: NodeState -> NodeExchange -> IO ()
handleNodeMsg nodeState msg =
    case msg of
        AddTransaction tx -> do
            txs <- readMVar (transactionPool nodeState)
            let hasTx = any (\t -> t == tx) txs
            if hasTx
                then putStrLn "Transaction aready exists"
                else do
                    modifyMVar_
                        (transactionPool nodeState)
                        (\txs -> pure $ tx : txs)
                    putStrLn $
                        "Transaction successsfully added. Total transactios: " <>
                        (show $ length txs + 1)
                    putStrLn $ "Writing transactios to channel"
                    writeChan (broadcastChannel nodeState) (TxBroadcast tx)
        QueryBlock n -> do
            blocks <- readMVar (blockchain nodeState)
            let block = find (\b -> Block.index b == n) blocks
            case block of
                Nothing -> pure ()
                (Just b) -> pure () -- (send . conversation) nodeState (BL.toStrict $ encode b)
        AddBlock block -> do
          putStrLn $ "Adding block " <> show (index block) <> " to the blockchain."
          -- Remove transaction from transaction pool which are in the newly added block
          let txs = transactions block
          modifyMVar_ (transactionPool nodeState) (\txs2 -> pure $ txs2 \\ txs)
          -- Add block itself
          modifyMVar_ (blockchain nodeState) (\blocks -> pure $ block <| blocks)

toExchange :: ByteString -> Exchange
toExchange = decode . BL.fromStrict

handleNodeExchange :: NodeState
                   -> NodeExchange
                   -> IO (ExchangeResponse, StateAction)
handleNodeExchange nodeState nodeExchange = do
    handleNodeMsg nodeState nodeExchange
    pure $ (NExchangeResp 1, NoAction)

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
    let blocksInfo = blockInfo <$> blockChain
    pure $
        NodeInfo
            (unNodeId ((nodeId . nodeConfig) nodeState))
            (length txSize)
            (length blockChain)
            (unNodeId <$> neighbours nodeState)
            blocksInfo

handleClientNodeExchange :: NodeState
                         -> ClientNodeExchange
                         -> IO (ExchangeResponse, StateAction)
handleClientNodeExchange nodeState clientNodeExchange =
    case clientNodeExchange of
        (MakeTransfer transfer signature) -> do
            case verifyTransfer signature transfer of
                True -> do
                    putStrLn $ "SIGNTURE VERYFIED " <> show transfer
                    timestamp <- now
                    let tx = Transaction transfer signature timestamp
                    pure $ (NExchangeResp 2, AddTransactionToNode tx)
                False -> pure $ (NExchangeResp 4, NoAction)
        (AskBalance address) -> pure $ (NExchangeResp 3, NoAction)
        (Register address) -> do
                    let genesisPk = (from . fst) genesisTransfer
                    let transfer = Transfer genesisPk address 1000
                    let signature = signTransfer (snd nodeKeyPair) transfer
                    timestamp <- now
                    let tx = Transaction transfer signature timestamp
                    pure $ ((StringResp "Registration successful"), AddTransactionToNode tx)
        FetchStatus -> do
            nodeInfo <- nodeStatus nodeState
            pure $ (StatusInfo nodeInfo, NoAction)


broadcastToExchange :: Broadcast -> NodeExchange
broadcastToExchange (TxBroadcast tx) = AddTransaction tx
broadcastToExchange (BlockBroadcast block) = AddBlock block

neighbourHandler :: Chan Broadcast -> NodeId -> Conversation -> IO ()
neighbourHandler broadcastChannel nodeId cc @ Conversation {..} = do
    putStrLn $
        "CONNECTED to nodeId " <> show (unNodeId nodeId) <>
        ", reading from channel"
    broadcast <- readChan broadcastChannel
    --putStrLn $ "TRANASACTIOB to boradcase " <> show broadcast
    response <- send (BL.toStrict $ encode (NExchange $ broadcastToExchange broadcast)) *> recv
    putStrLn $ "CONNECTED and receind nodeId " <> show (unNodeId nodeId)
    neighbourHandler broadcastChannel nodeId cc

connectToNeighbour :: Chan Broadcast -> NodeId -> IO ()
connectToNeighbour broadcastChan nodeId =
    try
        (connectToUnixSocket
             "sockets"
             nodeId
             (neighbourHandler broadcastChan nodeId)) >>=
    ssss broadcastChan nodeId

ssss :: Chan Broadcast -> NodeId -> Either IOException a -> IO ()
ssss broadcastChan nodeId (Right aa) =
    putStrLn $ "OK connection to " <> show (unNodeId nodeId) <> " successful!!!"
ssss broadcastChan nodeId (Left ex) = do
    putStrLn
        ("Connection to nodeId : " <> show (unNodeId nodeId) <> " failed: " <>
         show ex)
    putStrLn ("Attempting to reconnect in 10s")
    (threadDelay $ 10 * 1000 * 1000)
    putStrLn
        ("Trying reestablish connection with nodeId " <> show (unNodeId nodeId))
    connectToNeighbour broadcastChan nodeId

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
                  "Node " <> myId <> ". Forking new thread " <> show tId)) <* do
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
                "Node " <> myId <> " threadId " <> show tId <>
                " received some input " <>
                (show (length (BS.unpack input)))
            let exchange = toExchange input
            putStrLn $ "Receiver exhange" <> show exchange
            (er, action) <-
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
            send conversation (BL.toStrict (encode er))
          -- let ioAction =
          --         case processCommand command ledger of
          -- _ <- (try ioAction) >>= ssss2
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
    in putStrLn
           ("Node " <> myId <> ". " <> msg <> " with node: " <>
            show (unNodeId neighbourId))

connectToNeighbours :: NodeState -> IO [ThreadId]
connectToNeighbours nodeState =
    let nodeNeighbours = (neighbours nodeState)
        myId = nodeIdFromState nodeState
    in do sequence $
              (\nId ->
                   forkIO
                       (do logMessage myId nId "Trying to establish connecting"
                           myChan <- dupChan (broadcastChannel nodeState)
                           connectToNeighbour myChan nId
                           logMessage myId nId "Terminating connection")) <$>
              nodeNeighbours

listenForClientConnection :: NodeState -> IO ()
listenForClientConnection nodeState = do
    listenUnixSocket "sockets" (nodeIdFromState nodeState) (commu nodeState)
