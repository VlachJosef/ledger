{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Block as B
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Newtype
import Crypto.Sign.Ed25519
import Data.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Conversion as DBC
import Data.ByteString.Conversion.To
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import Data.Int
import Data.Map (Map)
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Exchange
import GHC.Generics
import NodeCommandLine
import Text.Read
import Transaction

import qualified Data.Map as Map
import Data.Semigroup
import Serokell.Communication.IPC

import qualified Network.Socket as Net

data LedgerError
    = AddressNotFound Address
    | NotEnoughBalance Address
                       Balance
    deriving (Eq, Show)

newtype TxId = TxId
    { unTxId :: Int
    } deriving (Eq, Ord, Show, Binary)

type Balance = Int

type MBalance = MVar Balance

newtype Ledger =
    Ledger (Map Address MBalance)
    deriving (Eq, Generic)

newtype LedgerFreeze =
    LedgerFreeze (Map Address Balance)
    deriving (Eq, Generic, Show)

newtype LedgerState =
    LedgerState (MVar Ledger)
    deriving (Eq, Generic)

data NodeState = NodeState
    { blockchain :: B.BlockChain
    }

freeze :: Ledger -> IO LedgerFreeze
freeze (Ledger ledger) = LedgerFreeze <$> traverse readMVar ledger

unFreeze :: LedgerFreeze -> IO Ledger
unFreeze (LedgerFreeze ledgerFreeze) = Ledger <$> traverse newMVar ledgerFreeze

instance Newtype Ledger

instance Binary LedgerFreeze

hash :: S.ByteString -> S.ByteString
hash = BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict

deriveAddress :: PublicKey -> Address
deriveAddress (PublicKey pk) =
    Address $ B58.encodeBase58 B58.bitcoinAlphabet (hash pk)

deriveAddressSK :: SecretKey -> Address
deriveAddressSK (SecretKey pk) =
    Address $ B58.encodeBase58 B58.bitcoinAlphabet (hash pk)

data Command
    = Submit Transfer
    | Query TxId
    | Balance Address
    | Register PublicKey
    | LedgerQuery
    | StatusQuery
    | MineBlock
    | LedgerSync
    | Sync
    | InvalidCommand S.ByteString

data CommandResult
    = MkTransfer Balance
                 MBalance
                 MBalance
    | LedgerStats
    | StatusStats
    | Registration Address
                   Balance
    | BalanceInfo MBalance
    | ErrorMessage String
    | RunSync
    | LedgerSyncRes
    | MineBlockRes

processCommand :: Command -> Ledger -> CommandResult
processCommand (Submit (Transfer {..})) ledger =
    either
        (\a -> ErrorMessage (show a))
        (\(fromM, toM) -> MkTransfer amount fromM toM)
        (transferBalance from to amount ledger)
processCommand (Query (TxId txId)) _ = ErrorMessage "TODO"
processCommand Sync _ = RunSync
processCommand (Balance address) ledger =
    either (\a -> ErrorMessage (show a)) (BalanceInfo) (balance ledger address)
processCommand (Register pk) _ = Registration (deriveAddress pk) 1000
processCommand LedgerQuery _ = LedgerStats
processCommand StatusQuery _ = StatusStats
processCommand LedgerSync _ = LedgerSyncRes
processCommand MineBlock _ = MineBlockRes
processCommand (InvalidCommand command) _ = ErrorMessage (show command)

stringToCommand :: [S.ByteString] -> IO Command
stringToCommand ["register"] = do
    (pk, sk) <- createKeypair
    pure $ Register pk
stringToCommand ["ledger"] = pure LedgerQuery
stringToCommand ["status"] = pure StatusQuery
stringToCommand ["mine"] = pure MineBlock
stringToCommand ["syncLedger"] = pure LedgerSync
stringToCommand ["sync"] = pure Sync
stringToCommand ["BALANCE", address] = pure $ Balance $ Address address
stringToCommand ["SUBMIT", from, to, amount] =
    let maybeInt = DBC.fromByteString amount :: Maybe Int
    in case maybeInt of
           Nothing ->
               pure $
               InvalidCommand ("Amount must be a number, got: " <> amount)
           (Just n) -> pure $ Submit $ Transfer (PublicKey from) (Address to) n
stringToCommand invalid = pure $ InvalidCommand (S.concat invalid)

balance :: Ledger -> Address -> Either LedgerError MBalance
balance (Ledger ledger) address =
    maybe (Left $ AddressNotFound address) Right (Map.lookup address ledger)

emptyLedger :: Ledger
emptyLedger = Ledger Map.empty

transferBalance :: PublicKey
                -> Address
                -> Int
                -> Ledger
                -> Either LedgerError (MBalance, MBalance)
transferBalance fromPk to amount ledger = do
    fromM <- balance ledger (deriveAddress fromPk)
    toM <- balance ledger to
    pure $ (fromM, toM)

response :: String -> S.ByteString
response str = BL.toStrict (toByteString (str <> "\n"))

ledgerSyncCmd = "syncLedger"

syncLedger :: NodeId -> LedgerState -> IO ()
syncLedger nodeId ledgerState =
    trace
        "syncLedger CALLED"
        (connectToUnixSocket "sockets" nodeId (synchronization ledgerState))

synchronization :: LedgerState -> Conversation -> IO ()
synchronization (LedgerState ledgerState) (Conversation {..}) = do
    tId <- myThreadId
    putStrLn ("IN synchronization ThreadId " <> show tId)
    send ledgerSyncCmd
    putStrLn ("IN synchronization ThreadId " <> show tId <> "AFTER SEND")
    ledger <- recv
    putStrLn ("IN synchronization ThreadId " <> show tId <> "AFTER RECV")
    let freezed = decode (BL.fromStrict ledger) :: LedgerFreeze
    ledger <- unFreeze freezed
    takeMVar ledgerState
    putMVar ledgerState ledger
    putStrLn ("TRACE synchronization ledger: " <> show freezed)

ssss :: Either IOException a -> IO ()
ssss (Right aa) = putStrLn "OK"
ssss (Left ex) = putStrLn ("KO: " <> show ex)

ssss2 :: Either IOException a -> IO ()
ssss2 (Right aa) = putStrLn "22OK"
ssss2 (Left ex) = putStrLn ("22KO: " <> show ex)
-- toExchange :: S.ByteString -> Exchange
-- toExchange = decode . BL.fromStrict
-- handleNodeExchange :: NodeExchange -> IO (ExchangeResponse, StateAction)
-- handleNodeExchange nodeExchange = pure $ (NExchangeResp 1, NoAction)
-- handleClientNodeExchange :: ClientNodeExchange
--                          -> IO (ExchangeResponse, StateAction)
-- handleClientNodeExchange clientNodeExchange =
--     case clientNodeExchange of
--         (MakeTransfer transfer signature) -> do
--             case verifyTransfer signature transfer of
--                 True -> do
--                     putStrLn $ "SIGNTURE VERYFIED " <> show transfer
--                     times <- now
--                     let tx = Transaction transfer signature times
--                     pure $ (NExchangeResp 2, AddTransactionToNode tx)
--                 False -> pure $ (NExchangeResp 4, NoAction)
--         (AskBalance address) -> pure $ (NExchangeResp 3, NoAction)
-- commu :: LedgerState -> NodeId -> Conversation -> IO Bool
-- commu (LedgerState ledgerState) nodeId (Conversation {..}) = do
--     True <$
--         forkIO
--             ((do tId <- myThreadId
--                  (putStrLn $
--                   "FORKING from " <> show tId <> ": NodeId " <> show nodeId)) <* do
--                  loop)
--   where
--     loop :: IO ()
--     loop = do
--         input <- recv
--         ledger <- readMVar ledgerState
--         tId <- myThreadId
--         putStrLn "Receiver some input"
--         let exchange = toExchange input
--         putStrLn $ "Receiver exhange" <> show exchange
--         (er, action) <-
--             case exchange of
--                 (NExchange nodeExchange) -> handleNodeExchange nodeExchange
--                 (CExchange clientNodeExchange) ->
--                     handleClientNodeExchange clientNodeExchange
--         case action of
--             NoAction -> pure ()
--             (AddTransactionToNode tx) -> pure ()
--         send (BL.toStrict (encode er))
--         nextStep (BS.unpack input) loop
-- dddds :: LedgerState -> NodeId -> Conversation -> IO Bool
-- dddds (LedgerState ledgerState) nodeId (Conversation {..}) = do
--     True <$
--         forkIO
--             ((do tId <- myThreadId
--                  (putStrLn $
--                   "FORKING from " <> show tId <> ": NodeId " <> show nodeId)) <* do
--                  loop)
--   where
--     loop :: IO ()
--     loop = do
--         input <- recv
--         ledger <- readMVar ledgerState
--         tId <- myThreadId
--         --putStrLn $ "LEDGER from thread " <> show tId <> ", Ledger: " <> show ledger
--         command <- stringToCommand $ BS.words input
--         --putStrLn (show tId <> " " <> show command)
--         let ioAction =
--                 case processCommand command ledger of
--                     RunSync ->
--                         (try (syncLedger (NodeId 1) (LedgerState ledgerState))) >>=
--                         ssss
--                     LedgerStats -> do
--                         freezed <- freeze ledger
--                         send $ response (show freezed)
--                     StatusStats -> do
--                         send $ response (show "HELLO FROM NODE AND I MEAN IT")
--                     BalanceInfo mBalance -> do
--                         balance <- readMVar mBalance
--                         send $ response ("Your balance is " <> show balance)
--                     Registration address balance -> do
--                         regBlanceMVar <- newMVar balance
--                         ledger <- takeMVar ledgerState
--                         putMVar
--                             ledgerState
--                             (over
--                                  Ledger
--                                  (Map.insert address regBlanceMVar)
--                                  ledger)
--                         send $ response ("Your address is " <> show address)
--                     MineBlockRes -> undefined -- B.mineBlock
--                     MkTransfer balance from to -> do
--                         f <- takeMVar from
--                         t <- takeMVar to
--                         putMVar from (f - balance)
--                         putMVar to (f + balance)
--                     LedgerSyncRes -> do
--                         freezed <- freeze ledger
--                         let ledg = BL.toStrict (encode freezed)
--                         trace
--                             ("TRACE [LedgerSyncRes] dddss tId" <> show tId <>
--                              "ledger " <>
--                              show ledg)
--                             (send ledg)
--                     ErrorMessage message ->
--                         send $ response ("INVALID " <> message)
--         _ <- (try ioAction) >>= ssss2
--         nextStep (BS.unpack input) loop
-- nextStep :: String -> IO () -> IO ()
-- nextStep "" io = putStrLn "Closed by peer!"
-- nextStep _ io = io
-- www :: CLIArguments -> IO ()
-- www (CLIArguments {..}) = do
--     ledgerState <- newMVar emptyLedger
--     listenUnixSocket "sockets" id (commu (LedgerState ledgerState) id)
