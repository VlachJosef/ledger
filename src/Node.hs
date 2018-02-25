{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Node
    ( establishClusterConnection
    , NodeState(..)
    , addBlock
    , initialNodeState
    ) where

import Node.Internal
import Address
import Data.Functor
import Block
import Control.Concurrent
import Control.Exception (try, IOException)
import Control.Logging
import Crypto.Sign.Ed25519 (Signature, PublicKey(..))
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Conversion as DBC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.List as List
import Data.List.NonEmpty( NonEmpty( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup
import Exchange
import Ledger
import NodeCommandLine
import Serokell.Communication.IPC
import System.Directory (doesFileExist, removeFile)
import Utils
import Transaction
import Text.PrettyPrint.Boxes as Boxes (render, vcat, hsep, left, text)
import Time
import Node.Data
import System.Posix.Signals

calculateNeighbours :: NodeConfig -> [NodeId]
calculateNeighbours nodeConfig =
    let nId = (unNodeId . nodeId) nodeConfig
    in NodeId <$> filter (\a -> a /= nId) [0 .. (nodeCount nodeConfig - 1)]

initialNodeState :: NodeConfig -> [(Address, Int)] -> IO NodeState
initialNodeState nodeConfig initialDistribution = do
    emptyBlockChain <- newMVar $ (genesisBlock initialDistribution) :| []
    emptyTransactionPool <- newMVar []
    ledger <- newMVar (Ledger (Map.fromList initialDistribution))
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
        Just addedBlock -> do
          writeChan (broadcastChannel nodeState) (BlockBroadcast addedBlock)
          let validCount = (length . transactions) addedBlock
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
                (\transactions -> pure $ tx : transactions)
            logThread $ "Transaction " <> show tx
              <> " successsfully added to the poll and to the broadcast. Total number of transactions in pool: "
              <> (show $ length txs + 1)
            writeChan (broadcastChannel nodeState) (TxBroadcast tx)
            pure NodeNoResponse
    QueryBlock n -> do
      blocks <- readMVar (blockchain nodeState)
      let block = find (\b -> Block.index b == n) blocks
      pure $ BlockResponse block
    AddBlock block -> do
      _ <- addBlock block nodeState
      pure NodeNoResponse

addBlock :: Block -> NodeState -> IO (Maybe Block)
addBlock block nodeState = let
     blocksM = blockchain nodeState
     ledgerM = nodeLedger nodeState
     txPoolM = transactionPool nodeState
  in do
  blocks <- takeMVar blocksM
  ledger <- takeMVar ledgerM
  case addReplaceBlock block nodeState blocks ledger of
    BlockAdded errors addedBlock ledgerUpd -> do
      logThread $ "Adding block " <> show (index block) <> " to the blockchain."
      -- Remove transactions from transaction pool which are in the newly added block
      modifyMVar_ txPoolM (\txs -> pure $ txs \\ transactions addedBlock)

      reportProblems errors
      putMVar ledgerM ledgerUpd
      putMVar blocksM (addedBlock <| blocks)
      pure $ Just addedBlock

    BlockNotAdded msg -> do
      putMVar ledgerM ledger
      putMVar blocksM blocks
      logThread msg
      pure Nothing

reportProblems :: [LedgerError] -> IO ()
reportProblems errors = sequence_ $ (logThread . show) <$> errors

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
          _ <- handleNodeExchange nodeState (AddTransaction tx)
          pure $ SubmitResp $ Just transactionId
        else pure $ SubmitResp Nothing

    AskBalance address -> do
      Ledger ledger <- readMVar (nodeLedger nodeState)
      pure $ case Map.lookup address ledger of
        Nothing -> StringResp $ "Balance error, unknown address: " <> show address
        Just bal -> BalanceResp bal

    Query txId -> do
      blocks <- readMVar (blockchain nodeState)
      let wasAdded = transactionIdInBlockchain txId blocks
      pure $ QueryResp wasAdded

    FetchStatus -> do
      nodeInfo <- nodeStatus nodeState
      pure $ StatusInfo nodeInfo

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
      _ <- addBlock block nodeState
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
encodeNodeExchange = BSL.toStrict . encode . NodeExchange

connectToNeighbour :: NodeState -> Chan Broadcast -> NodeId -> IO ()
connectToNeighbour nodeState broadcastChan nodeId =
    try
        (connectToUnixSocket
             "sockets"
             nodeId
             (neighbourHandler nodeState broadcastChan nodeId)) >>=
    retry nodeState broadcastChan nodeId

retry :: NodeState -> Chan Broadcast -> NodeId -> Either IOException a -> IO ()
retry nodeState broadcastChan nodeId = let
  nId = showNodeId nodeId
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
nodeIdFromStateStr = showNodeId . nodeIdFromState

commu :: NodeState -> Conversation -> IO Bool
commu nodeState conversation = do
    True <$ forkIO ((logThread $ "node " <> myId <> ". Forking new thread!") <* loopMain nodeState)
  where
    myId = nodeIdFromStateStr nodeState
    loopMain :: NodeState -> IO ()
    loopMain ns = loop
      where
        loop :: IO ()
        loop = do
            input <- recv conversation
            let exchange = decodeExchange input
            logThread $ "node " <> myId <> " received " <> show exchange
            exchangeResponse <-
                case exchange of
                    NodeExchange nodeExchange ->
                       encode <$> handleNodeExchange ns nodeExchange
                    ClientExchange clientNodeExchange ->
                       encode <$> handleClientExchange ns clientNodeExchange

            send conversation (BSL.toStrict exchangeResponse)
            nextStep (BSC.unpack input) loop

startMinerThread :: NodeState -> IO ()
startMinerThread = void . forkIO . mineblock

parseAddressAndAmount :: BSC.ByteString -> Maybe (Address, Int)
parseAddressAndAmount bs = case BSC.words bs of
  pk : amountAsStr : [] ->
    case DBC.fromByteString amountAsStr of
      Just n -> Just (Address pk, n)
      Nothing -> Nothing
  _ -> Nothing

readDistributionFile :: NodeConfig -> IO (Maybe [(Address, Int)])
readDistributionFile nodeConfig = do
    let dFile = distributionFile nodeConfig
    fileExists <- doesFileExist dFile
    if fileExists
        then do
            keysBS <- BSC.readFile dFile
            pure $ sequence $ parseAddressAndAmount <$> BSC.lines keysBS
        else pure Nothing

establishClusterConnection :: NodeConfig -> IO ()
establishClusterConnection nodeConfig = let
  nId = (showNodeId . nodeId) nodeConfig
  in withFileLogging ("log/node-" <> nId <> ".log" ) $ do
    dFile <- readDistributionFile nodeConfig
    void $ installHandler sigTERM (terminationHandler nodeConfig sigTERM) Nothing
    void $ installHandler sigINT  (terminationHandler nodeConfig sigINT)  Nothing
    case dFile of
      Just initialDistribution -> do
        nodeState <- initialNodeState nodeConfig initialDistribution
        startMinerThread nodeState
        _ <- connectToNeighbours nodeState
        listenForClientConnection nodeState
      Nothing -> logThread "Error when reading distribution file."

logMessage :: NodeId -> NodeId -> String -> IO ()
logMessage nodeId neighbourId msg =
   let myId = showNodeId nodeId
       nId = showNodeId neighbourId
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
                           logMessage myId nId "Terminating connection")) <$> nodeNeighbours

listenForClientConnection :: NodeState -> IO ()
listenForClientConnection nodeState = do
    listenUnixSocket "sockets" (nodeIdFromState nodeState) (commu nodeState)

terminationHandler :: NodeConfig -> Signal ->  Handler
terminationHandler nodeConfig signal = CatchOnce $ do
    logThread $ "Caught " <> show signal <> " signal."
    let socketFile = "sockets/" <> ((show . unNodeId . nodeId) nodeConfig) <> ".sock"
    fileExists <- doesFileExist socketFile
    if fileExists
      then do
        logThread $ "Removing socket file: " <> socketFile
        removeFile socketFile
      else logThread $ "File: " <> socketFile <> " not found."
    flushLog
    raiseSignal signal
