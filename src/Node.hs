{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Node
    ( establishClusterConnection
    ) where

import Block
import Control.Concurrent
import Control.Exception
import Crypto.Sign.Ed25519 (Signature)
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Semigroup
import Exchange
import qualified GHC.Generics as G
import Ledger
import NodeCommandLine
import Serokell.Communication.IPC
import Transaction

data NodeState = NodeState
    { nodeConfig :: NodeConfig
    , neighbours :: [NodeId]
    , blockchain :: MVar BlockChain
    , transactionPool :: MVar [Transaction]
    , conversation :: Conversation
    , nodeLedger :: MVar Ledger
    }

calculateNeighbours :: NodeConfig -> [NodeId]
calculateNeighbours nodeConfig =
    let nId = (unNodeId . nodeId) nodeConfig
    in NodeId <$> filter (\a -> a /= nId) [0 .. nodeCount nodeConfig]

initialNodeState :: NodeConfig -> Conversation -> IO NodeState
initialNodeState nodeConfig conversation = do
    emptyBlockChain <- newMVar []
    emptyTransactionPool <- newMVar []
    ledger <- newMVar emptyLedger
    pure $
        NodeState
            nodeConfig
            (calculateNeighbours nodeConfig)
            emptyBlockChain
            emptyTransactionPool
            conversation
            ledger

mineblock :: NodeState -> Block
mineblock nodeState = undefined

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
        QueryBlock n -> do
            blocks <- readMVar (blockchain nodeState)
            let block = find (\b -> Block.index b == n) blocks
            case block of
                Nothing -> pure ()
                (Just b) ->
                    (send . conversation) nodeState (BL.toStrict $ encode b)
        AddBlock block -> undefined

toExchange :: ByteString -> Exchange
toExchange = decode . BL.fromStrict

handleNodeExchange :: NodeState
                   -> NodeExchange
                   -> IO (ExchangeResponse, StateAction)
handleNodeExchange nodeState nodeExchange = do
    handleNodeMsg nodeState nodeExchange
    pure $ (NExchangeResp 1, NoAction)

nodeStatus :: NodeState -> IO NodeInfo
nodeStatus nodeState = do
    txSize <- readMVar (transactionPool nodeState)
    blockChainSize <- readMVar (blockchain nodeState)
    pure $
        NodeInfo
            (unNodeId ((nodeId . nodeConfig) nodeState))
            (length txSize)
            (length blockChainSize)
            (unNodeId <$> neighbours nodeState)

handleClientNodeExchange :: NodeState
                         -> ClientNodeExchange
                         -> IO (ExchangeResponse, StateAction)
handleClientNodeExchange nodeState clientNodeExchange =
    case clientNodeExchange of
        (MakeTransfer transfer signature) -> do
            case verifyTransfer signature transfer of
                True -> do
                    putStrLn $ "SIGNTURE VERYFIED " <> show transfer
                    times <- now
                    let tx = Transaction transfer signature times
                    pure $ (NExchangeResp 2, AddTransactionToNode tx)
                False -> pure $ (NExchangeResp 4, NoAction)
        (AskBalance address) -> pure $ (NExchangeResp 3, NoAction)
        FetchStatus -> do
            nodeInfo <- (nodeStatus nodeState)
            pure $ (StatusInfo nodeInfo, NoAction)

neighbourHandler :: NodeId -> Conversation -> IO ()
neighbourHandler nodeId Conversation {..} = do
    putStrLn $ "CONNECTED to nodeId " <> show (unNodeId nodeId)
    x <- recv
    putStrLn $ "CONNECTED and receind nodeId " <> show (unNodeId nodeId)
    pure ()

connectToNeighbour :: NodeId -> IO ()
connectToNeighbour nodeId =
    try (connectToUnixSocket "sockets" nodeId (neighbourHandler nodeId)) >>=
    ssss nodeId

ssss :: NodeId -> Either IOException a -> IO ()
ssss nodeId (Right aa) =
    putStrLn $ "OK connection to " <> show (unNodeId nodeId) <> " successful!!!"
ssss nodeId (Left ex) = do
    putStrLn
        ("Connection to nodeId : " <> show (unNodeId nodeId) <> " failed: " <>
         show ex)
    putStrLn ("Attempting to reconnect in 10s")
    (threadDelay $ 10 * 1000 * 1000)
    putStrLn
        ("Trying reestablish connection with nodeId " <> show (unNodeId nodeId))
    connectToNeighbour nodeId

commu :: NodeConfig -> Conversation -> IO Bool
commu nc (cc@Conversation {..}) = do
    nodeState <- initialNodeState nc cc
    let myId = show (unNodeId $ nodeId nc)
    True <$ do
        forkIO
            ((do tId <- myThreadId
                 (putStrLn $
                  "Node " <> myId <> ". Forking new thread " <> show tId)) <* do
                 loopO nodeState)
  where
    loopO :: NodeState -> IO ()
    loopO nodeState = loop
      where
        loop :: IO ()
        loop = do
            input <- recv
            tId <- myThreadId
            putStrLn $
                "Node " <> (show . unNodeId . nodeId . nodeConfig) nodeState <>
                " threadId " <>
                show tId <>
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
            send (BL.toStrict (encode er))
          -- let ioAction =
          --         case processCommand command ledger of
          -- _ <- (try ioAction) >>= ssss2
            nextStep (BS.unpack input) loop

nextStep :: String -> IO () -> IO ()
nextStep "" io = putStrLn "Closed by peer!"
nextStep _ io = io

establishClusterConnection :: NodeConfig -> IO ()
establishClusterConnection nodeConfig = do
    connectToNeighbours nodeConfig
    listenForClientConnection nodeConfig

logMessage :: NodeId -> NodeId -> String -> IO ()
logMessage nodeId neighbourId msg =
    let myId = show (unNodeId nodeId)
    in putStrLn
           ("Node " <> myId <> ". " <> msg <> " with node: " <>
            show (unNodeId neighbourId))

connectToNeighbours :: NodeConfig -> IO [ThreadId]
connectToNeighbours nodeConfig =
    let nodeNeighbours = (calculateNeighbours nodeConfig)
        myId = nodeId nodeConfig
    in do sequence $
              (\nId ->
                   forkIO
                       (do logMessage myId nId "Trying to establish connecting"
                           connectToNeighbour nId
                           logMessage myId nId "Terminating connection")) <$>
              nodeNeighbours

listenForClientConnection :: NodeConfig -> IO ()
listenForClientConnection nodeConfig =
    listenUnixSocket "sockets" (nodeId nodeConfig) (commu nodeConfig)
