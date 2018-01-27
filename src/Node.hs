{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Node
    ( www
    ) where

import Block
import Control.Concurrent
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
import Serokell.Communication.IPC
import Transaction

data NodeState = NodeState
    { nodeId :: NodeId
    , neighbours :: [NodeId]
    , blockchain :: MVar BlockChain
    , transactionPool :: MVar [Transaction]
    , conversation :: Conversation
    , nodeLedger :: MVar Ledger
    }

initialNodeState :: NodeId -> Conversation -> IO NodeState
initialNodeState nodeId conversation = do
    emptyBlockChain <- newMVar []
    emptyTransactionPool <- newMVar []
    ledger <- newMVar emptyLedger
    pure $
        NodeState
            nodeId
            []
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
            (unNodeId (nodeId nodeState))
            (length txSize)
            (length blockChainSize)

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

commu :: NodeId -> Conversation -> IO Bool
commu nodeId (cc@Conversation {..}) = do
    nodeState <- initialNodeState nodeId cc
    True <$
        forkIO
            ((do tId <- myThreadId
                 (putStrLn $
                  "FORKING from " <> show tId <> ": NodeId " <>
                  show (unNodeId nodeId))) <* do loopO nodeState)
  where
    loopO :: NodeState -> IO ()
    loopO nodeState = loop
      where
        loop :: IO ()
        loop = do
            input <- recv
            tId <- myThreadId
            putStrLn "Receiver some input"
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

www :: NodeId -> IO ()
www nodeId = listenUnixSocket "sockets" nodeId (commu nodeId)
