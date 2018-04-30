{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Node
    ( establishClusterConnection
    , NodeState(..)
    , addBlock
    , initialNodeState
    ) where

import           Address                    (Address (..), deriveAddress)
import           Block                      (Block (..), BlockChain, Index (..), genesisBlock, index, nextIndex,
                                             timestamp)
import           Control.Concurrent         (Chan, ThreadId, dupChan, forkIO, modifyMVar_, newChan, newMVar, putMVar,
                                             readChan, readMVar, takeMVar, writeChan)
import           Control.Concurrent.Async   (concurrently_)
import           Control.Exception          (IOException, try)
import           Control.Logging            (flushLog, withFileLogging)
import           Crypto.Sign.Ed25519        (Signature, dsign, toPublicKey)
import           Data.Bifunctor             (bimap)
import           Data.Binary                (encode)
import           Data.Binary.Get            (ByteOffset)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Conversion as DBC
import qualified Data.ByteString.Lazy       as BSL
import           Data.Functor               (void)
import           Data.List                  as List
import           Data.List.NonEmpty         (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty         as NEL
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import           Exchange                   (Broadcast (..), ClientExchange (..), ClientExchangeCLI (..),
                                             ClientExchangeCLIResponse (..), ClientExchangeResponse (..), Exchange (..),
                                             NodeExchange (..), NodeExchangeResponse (..), NodeInfo (..),
                                             decodeExchange, decodeNodeExchange, decodeNodeExchangeResponse,
                                             decodeSubmitResp, runPutStrict)
import           Ledger                     (Ledger (..), LedgerError)
import           Node.Data                  (AddBlockRes (..), NodeState (..), fetchNodeId, toInterNodeId)
import           Node.Internal              (addReplaceBlock)
import           NodeCommandLine            (NodeConfig, distributionFile, nodeCount, nodeId, stabilityTimeout)
import           Serokell.Communication.IPC (Conversation (..), NodeId (..), connectToUnixSocket, listenUnixSocket)
import           System.Directory           (doesFileExist, removeFile)
import           System.Posix.Signals       (Handler (..), Signal, installHandler, raiseSignal, sigINT, sigTERM)
import           Text.PrettyPrint.Boxes     as Boxes (hsep, left, render, text, vcat)
import           Time.Units                 (Millisecond, Time, ms, threadDelay, toNum)
import           Transaction                (InitiateTransfer (..), Transaction (..), TransactionId (..), Transfer (..),
                                             amount, encodeTransfer, from, to, transactionId, transfer)
import           Utils                      (encodeSignature, logThread, now, recvAll, showNodeId)

newtype DecodingError = DecodingError String

calculateNeighbours :: NodeConfig -> [NodeId]
calculateNeighbours nodeConfig =
    let nId = (unNodeId . nodeId) nodeConfig
    in NodeId . (+1000) <$> filter (/= nId) [0 .. (nodeCount nodeConfig - 1)]

initialNodeState :: NodeConfig -> [(Address, Int)] -> IO NodeState
initialNodeState nodeConfig initialDistribution = do
    emptyBlockChain <- newMVar $ genesisBlock initialDistribution :| []
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

    miningDelay :: Time Millisecond
    miningDelay = ms . fromIntegral . (`div` 10) . toNum @Millisecond . stabilityTimeout . nodeConfig $ nodeState

    loop = do
      threadDelay miningDelay
      txs <- readMVar $ transactionPool nodeState
      if null txs then logThread "No Transaction to mine." else mine txs
      loop

    mine :: [Transaction] -> IO ()
    mine txs = do
      chain <- readMVar $ blockchain nodeState
      timestamp <- now
      let lastBlock = NEL.head chain
      let nextBlockId = nextIndex (index lastBlock)
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
  or $ elem transaction . transactions <$> blocks

transactionIdInBlockchain :: TransactionId -> BlockChain -> Bool
transactionIdInBlockchain txId blocks =
  or $ elem txId . map transactionId . transactions <$> blocks

handleNodeExchange :: NodeState -> NodeExchange -> IO NodeExchangeResponse
handleNodeExchange nodeState =
  \case
    AddTransaction tx -> do
      blocks <- readMVar $ blockchain nodeState
      txs <- readMVar $ transactionPool nodeState
      let hasTx = tx `elem` txs
      if hasTx || transactionInBlockchain tx blocks
        then do logThread $ "Transaction already exists: " <> show tx
                pure NodeNoResponse
        else do
            modifyMVar_
                (transactionPool nodeState)
                (\transactions -> pure $ tx : transactions)
            logThread $ "Transaction " <> show tx
              <> " successsfully added to the poll and to the broadcast. Total number of transactions in pool: "
              <> show (length txs + 1)
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
    BlockAdded errors addedBlock ledgerUpd blocksUpdated -> do
      logThread $ "Adding block " <> show (index block) <> " to the blockchain. Blockchain size: " <> show (NEL.length blocks)
      -- Remove transactions from transaction pool which are in the newly added block
      modifyMVar_ txPoolM (\txs -> pure $ txs \\ transactions addedBlock)

      reportProblems errors
      putMVar ledgerM ledgerUpd
      putMVar blocksM (addedBlock <| blocksUpdated)
      pure $ Just addedBlock

    BlockNotAdded msg -> do
      putMVar ledgerM ledger
      putMVar blocksM blocks
      logThread msg
      pure Nothing

reportProblems :: [LedgerError] -> IO ()
reportProblems errors = sequence_ $ logThread . show <$> errors

txInfo :: Transaction -> [String]
txInfo tx =
  ($tx) <$> [ show . deriveAddress . from . transfer
            , show . to . transfer
            , show . amount . transfer
            , show . transactionId]

txsInfo :: [Transaction] -> [[String]]
txsInfo txs = txInfo <$> txs

blockInfo :: Block -> String
blockInfo block = let
  blockIndex = (show . index) block
  txs = (txsInfo . transactions) block
  txsWithBlockId = txs ++
    [["Index    : " <> blockIndex]] ++
    [["Timestamp: " <> (show . Block.timestamp) block]]
  in render $ hsep 2 left (map (vcat left . map Boxes.text) (transpose txsWithBlockId))

nodeStatus :: NodeState -> IO NodeInfo
nodeStatus nodeState = do
    txSize <- readMVar (transactionPool nodeState)
    blocks <- readMVar (blockchain nodeState)
    ledger <- readMVar (nodeLedger nodeState)
    let blocksInfo = blockInfo <$> blocks
    pure $
        NodeInfo
            ((unNodeId . fetchNodeId) nodeState)
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
    MakeTransfer InitiateTransfer{..} -> do
        timestamp <- now
        let
          transfer      = Transfer (toPublicKey itFrom) (deriveAddress itTo) itAmount
          signature     = dsign itFrom ((toByteString . nodeId . nodeConfig $ nodeState) <> toByteString timestamp <> encodeTransfer transfer)
          transactionId = createTransaction signature
          tx            = Transaction transactionId transfer signature timestamp
        void $ handleNodeExchange nodeState (AddTransaction tx)
        pure $ SubmitResp $ Just transactionId

    AskBalance pk -> askBalance nodeState BalanceResp . deriveAddress $ pk

    Query txId -> do
      blocks <- readMVar (blockchain nodeState)
      let wasAdded = transactionIdInBlockchain txId blocks
      pure $ QueryResp wasAdded
    where
      toByteString :: Show a => a -> ByteString
      toByteString = BSC.pack . show

handleClientExchangeCLI :: NodeState
                        -> ClientExchangeCLI
                        -> IO ClientExchangeCLIResponse
handleClientExchangeCLI nodeState =
  \case
    AskBalanceByAddress address -> askBalance nodeState BalanceRespCLI address

    FetchStatus -> do
      nodeInfo <- nodeStatus nodeState
      pure $ StatusInfo nodeInfo

askBalance :: NodeState -> (Int -> a) -> Address -> IO a
askBalance nodeState f address = do
   Ledger ledger <- readMVar (nodeLedger nodeState)
   pure . f $ fromMaybe 0 $ Map.lookup address ledger

broadcastToExchange :: Broadcast -> NodeExchange
broadcastToExchange = \case
  TxBroadcast tx       -> AddTransaction tx
  BlockBroadcast block -> AddBlock block

synchonizeBlockChain :: NodeState -> Index -> Conversation -> NodeId -> IO ()
synchonizeBlockChain nodeState idx cc@Conversation {..} nId = do
  logMessage $ "Asking for block " <> show idx
  response <- send (encodeNodeExchange (fetchNodeId nodeState) (QueryBlock idx)) *> recvAll recv
  case decodeNodeExchangeResponse response of
    Right (_, _, BlockResponse (Just block)) -> do
      logMessage $ "Block number " <> show idx <> " received. Adding it to blockchain"
      _ <- addBlock block nodeState
      synchonizeBlockChain nodeState (nextIndex idx) cc nId
    Right (_, _, BlockResponse Nothing) -> logMessage $ "Block number " <> show idx <> " don't received. Synchronization complete."
    other -> logMessage $ "Error: Expected BlockResponse got: " <> show other
    where
      logMessage msg = logThread $ "[Synchronizing " <> show nId  <> "] " <> msg

neighbourHandler :: NodeState -> Chan Broadcast -> NodeId -> Conversation -> IO ()
neighbourHandler nodeState broadcastChannel nId cc@Conversation {..} = do
  blocks <- readMVar (blockchain nodeState)
  synchonizeBlockChain nodeState (nextIndex . Index $ List.length blocks) cc nId
  loop where
  loop = do
    broadcast <- readChan broadcastChannel
    logMessage $ "to send: " <> show broadcast
    response  <- send (encodeNodeExchange (fetchNodeId nodeState) (broadcastToExchange broadcast)) *> recvAll recv
    let dec = decodeNodeExchangeResponse response
    logMessage $ "recieved: " <> show dec
    loop
  logMessage msg = logThread $ "[Neighbour Handler " <> show nId  <> "] " <> msg

encodeNodeExchange :: NodeId -> NodeExchange -> ByteString
encodeNodeExchange nId = BSL.toStrict . encode -- TODO propagate nId

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
    logThread "Will attempt to reconnect in 100ms."
    threadDelay @Millisecond 100
    logThread $ "Trying reestablish connection with nodeId " <> nId
    connectToNeighbour nodeState broadcastChan nodeId

clientExchangeHandler :: NodeState -> ByteString -> Either DecodingError (IO ByteString)
clientExchangeHandler ns bs = logAndEncode ns <$> toClientExchange bs
    where
      logAndEncode :: NodeState -> Exchange -> IO ByteString
      logAndEncode ns exchange = do
        logThread $ "[ClientNodeCommunication handler] Received: " <> show exchange
        case exchange of
          ClientExchange clientNodeExchange ->
              runPutStrict . decodeSubmitResp <$> handleClientExchange ns clientNodeExchange
          ClientExchangeCLI clientNodeExchangeCLI ->
              BSL.toStrict . encode <$> handleClientExchangeCLI ns clientNodeExchangeCLI

toClientExchange :: ByteString -> Either DecodingError Exchange
toClientExchange input = bimap decodingFailure resultOnly (decodeExchange input)

decodingFailure :: (BSL.ByteString, ByteOffset, String) -> DecodingError
decodingFailure (bytestring, offset, errorMessage) =
  DecodingError $ "Cannot decode message: " <> show errorMessage <> ", offset: " <> show offset <> ", payload: " <> show bytestring

resultOnly :: (BSL.ByteString, ByteOffset, a) -> a
resultOnly (_, _, a) = a

nodeExchangeHandler :: NodeState -> ByteString -> Either DecodingError (IO ByteString)
nodeExchangeHandler ns bs = logAndEncode ns <$> toNodeExchange bs
    where
      logAndEncode :: NodeState -> NodeExchange -> IO ByteString
      logAndEncode ns nodeExchange = do
        logThread $ "[InterNodeCommunication handler] Received: " <> show nodeExchange
        BSL.toStrict . encode <$> handleNodeExchange ns nodeExchange

toNodeExchange :: ByteString -> Either DecodingError NodeExchange
toNodeExchange input = bimap decodingFailure resultOnly (decodeNodeExchange input)

communication :: (NodeState -> ByteString -> Either DecodingError (IO ByteString)) -> NodeState -> Conversation -> IO Bool
communication inputHandler nodeState conversation =
    True <$ forkIO (logThread "Forking new thread!" <* loopMain nodeState)
  where
    loopMain :: NodeState -> IO ()
    loopMain ns = loop
      where
        loop :: IO ()
        loop = do
            input <- recvAll (recv conversation)
            if null (BSC.unpack input)
              then logThread "Closed by peer!!!"
              else
                case inputHandler ns input of
                  Right decodedResp -> do
                    response <- decodedResp
                    send conversation response
                    loop
                  Left (DecodingError err) -> do
                    logThread $ "Error: " <> err
                    send conversation "error" -- let's send something back, as opposite side is waiting for something to receive
                    loop

startMinerThread :: NodeState -> IO ()
startMinerThread = void . forkIO . mineblock

parseAddressAndAmount :: BSC.ByteString -> Maybe (Address, Int)
parseAddressAndAmount bs = case BSC.words bs of
  [pk, amountAsStr] ->
    case DBC.fromByteString amountAsStr of
      Just n  -> Just (Address pk, n)
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
        (void . connectToNeighbours) nodeState
        listenForClientConnection nodeState
      Nothing -> logThread "Error when reading distribution file."

connectToNeighbours :: NodeState -> IO [ThreadId]
connectToNeighbours nodeState =
    let nodeNeighbours = neighbours nodeState
    in sequence $
              (\nId ->
                   forkIO
                       (do logMessage nId "Trying to establish connecting"
                           myChan <- dupChan (broadcastChannel nodeState)
                           connectToNeighbour nodeState myChan nId
                           logMessage nId "Terminating connection")) <$> nodeNeighbours where
            logMessage :: NodeId -> String -> IO ()
            logMessage neighbourId msg =
              let nId = showNodeId neighbourId
              in logThread $ msg <> " with node: " <> nId

listenForClientConnection :: NodeState -> IO ()
listenForClientConnection nodeState =
    concurrently_
      (listenUnixSocket "sockets" (fetchNodeId nodeState)                 (communication clientExchangeHandler nodeState))
      (listenUnixSocket "sockets" (toInterNodeId $ fetchNodeId nodeState) (communication nodeExchangeHandler   nodeState))

terminationHandler :: NodeConfig -> Signal ->  Handler
terminationHandler nodeConfig signal = CatchOnce $ do
    logThread $ "Caught " <> show signal <> " signal."
    let nId = nodeId nodeConfig
    removeSocketFile nId
    removeSocketFile $ toInterNodeId nId
    flushLog
    raiseSignal signal

    where
      removeSocketFile :: NodeId -> IO ()
      removeSocketFile nodeId =
        let socketFile = "sockets/" <> show nodeId <> ".sock"
        in do
          fileExists <- doesFileExist socketFile
          if fileExists
            then do
              logThread $ "Removing socket file: " <> socketFile
              removeFile socketFile
            else logThread $ "File: " <> socketFile <> " not found."
