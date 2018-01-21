{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import CommandLine
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
import Data.Map (Map)
import Data.Maybe
import Data.Tuple
import Debug.Trace
import GHC.Generics
import Text.Read

import qualified Data.Map as Map
import Data.Semigroup
import Serokell.Communication.IPC

import qualified Network.Socket as Net

data LedgerError
    = AddressNotFound Address
    | NotEnoughBalance Address
                       Balance
    deriving (Eq, Show)

data Transaction = Transaction
    { from :: Address
    , to :: Address
    , amount :: Int
    } deriving (Show)

newtype TxId = TxId
    { unTxId :: Int
    } deriving (Eq, Ord, Show, Binary)

newtype Address = Address
    { rawAddress :: S.ByteString
    } deriving (Show, Read, Eq, Ord, Monoid, Binary)

type Balance = Int

newtype Example =
    Example Int
    deriving (Generic)

instance Newtype Example

newtype Ledger =
    Ledger (Map Address Balance)
    deriving (Eq, Show, Generic)

newtype LedgerState =
    LedgerState (MVar Ledger)
    deriving (Eq, Generic)

instance Newtype Ledger

instance Binary Ledger

hash :: S.ByteString -> S.ByteString
hash = BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict

deriveAddress :: PublicKey -> Address
deriveAddress (PublicKey pk) =
    Address $ B58.encodeBase58 B58.bitcoinAlphabet (hash pk)

data Command
    = Submit Transaction
    | Query TxId
    | Balance Address
    | Register PublicKey
    | LedgerQuery
    | LedgerSync
    | Sync
    | InvalidCommand S.ByteString
    deriving (Show)

data CommandResult
    = LedgerUpdate Ledger
    | Registration Address
                   Ledger
    | BalanceInfo Balance
    | NoAction String
    | RunSync
    | LedgerSyncRes S.ByteString
    deriving (Show)

processCommand :: Ledger -> Command -> CommandResult
processCommand ledger (Submit (Transaction {..})) =
    either
        (\a -> NoAction (show a))
        (LedgerUpdate)
        (transferBalance from to amount ledger)
processCommand ledger (Query (TxId txId)) = NoAction ""
processCommand ledger Sync = RunSync
processCommand ledger (Balance address) =
    either (\a -> NoAction (show a)) (BalanceInfo) (balance ledger address)
processCommand ledger (Register pk) = Registration address ledgerUpd
  where
    (address, ledgerUpd) = register ledger pk
processCommand ledger (LedgerQuery) = LedgerUpdate ledger
processCommand ledger (LedgerSync) =
    LedgerSyncRes $ BL.toStrict (encode (ssww ledger))
processCommand ledger (InvalidCommand command) = NoAction (show command)

ssww :: Ledger -> Ledger
ssww = over Ledger (Map.insert (Address "Dsdasdsa") 1000)

stringToCommand :: [S.ByteString] -> IO Command
stringToCommand ["register"] = do
    (pk, sk) <- createKeypair
    pure $ Register pk
stringToCommand ["ledger"] = pure LedgerQuery
stringToCommand ["syncLedger"] = pure LedgerSync
stringToCommand ["sync"] = pure Sync
stringToCommand ["BALANCE", address] = pure $ Balance $ Address address
stringToCommand ["SUBMIT", from, to, amount] =
    let maybeInt = DBC.fromByteString amount :: Maybe Int
    in case maybeInt of
           Nothing ->
               pure $
               InvalidCommand ("Amount must be a number, got: " <> amount)
           (Just n) -> pure $ Submit $ Transaction (Address from) (Address to) n
stringToCommand invalid = pure $ InvalidCommand (S.concat invalid)

register :: Ledger -> PublicKey -> (Address, Ledger)
register ledger pk = (address, ledgerUpd)
  where
    address = deriveAddress pk
    ledgerUpd = initialBalance ledger address

balance :: Ledger -> Address -> Either LedgerError Balance
balance (Ledger ledger) address =
    maybe (Left $ AddressNotFound address) Right (Map.lookup address ledger)

emptyLedger :: Ledger
emptyLedger = Ledger Map.empty

addBalance :: Address -> Int -> Ledger -> Either LedgerError Ledger
addBalance address amount ledger = do
    currentBalance <- balance ledger address
    pure $ over Ledger (Map.insert address (currentBalance + amount)) ledger

withdrawBalance :: Address -> Int -> Ledger -> Either LedgerError Ledger
withdrawBalance address amount ledger = do
    currentBalance <- balance ledger address
    if currentBalance < amount
        then Left $ NotEnoughBalance address currentBalance
        else Right $
             over Ledger (Map.insert address (currentBalance - amount)) ledger

transferBalance :: Address
                -> Address
                -> Int
                -> Ledger
                -> Either LedgerError Ledger
transferBalance from to amount =
    addBalance to amount <=< withdrawBalance from amount

initialBalance :: Ledger -> Address -> Ledger
initialBalance ledger address = over Ledger (Map.insert address 1000) ledger

response :: String -> S.ByteString
response str = BL.toStrict (toByteString (str <> "\n"))

syncLedger :: NodeId -> IO ()
syncLedger nodeId =
    trace
        "syncLedger CALLED"
        (connectToUnixSocket "sockets" nodeId synchronization)

ledgerSyncCmd = "syncLedger"

synchronization :: Conversation -> IO ()
synchronization (Conversation {..})
    -- pure () <$ forkIO $
 = do
    tId <- myThreadId
    putStrLn ("IN synchronization ThreadId " <> show tId)
    send ledgerSyncCmd
    putStrLn ("IN synchronization ThreadId " <> show tId <> "AFTER SEND")
    let ssss = trace ("TRACE synchronization") ()
    ledger <- recv
    putStrLn ("IN synchronization ThreadId " <> show tId <> "AFTER RECV")
    let ledgerObj = decode (BL.fromStrict ledger) :: Ledger
    putStrLn ("TRACE synchronization ledger: " <> show ledgerObj)
    pure ()

ssss :: Either IOException a -> IO ()
ssss (Right aa) = putStrLn "OK"
ssss (Left ex) = putStrLn ("KO: " <> show ex)

ssss2 :: Either IOException a -> IO ()
ssss2 (Right aa) = putStrLn "22OK"
ssss2 (Left ex) = putStrLn ("22KO: " <> show ex)

dddds :: LedgerState -> NodeId -> Conversation -> IO Bool
dddds (LedgerState ledgerState) nodeId (Conversation {..}) = do
    True <$
        forkIO
            ((do tId <- myThreadId
                 (putStrLn $
                  "FORKING from " <> show tId <> ": NodeId " <> show nodeId)) <* do
                 loop)
  where
    loop :: IO ()
    loop = do
        input <- recv
        ledger <- readMVar ledgerState
        tId <- myThreadId
        --putStrLn $ "LEDGER from thread " <> show tId <> ", Ledger: " <> show ledger
        command <- stringToCommand $ BS.words input
        putStrLn (show tId <> " " <> show command)
        let (ledgerUpd, ioAction) =
                case processCommand ledger command of
                    RunSync -> (ledger, (try (syncLedger (NodeId 1))) >>= ssss)
                    LedgerUpdate ledger ->
                        (ledger, send $ response (show ledger))
                    BalanceInfo balance ->
                        ( ledger
                        , send $ response ("Your balance is " <> show balance))
                    Registration address ledger ->
                        ( ledger
                        , send $ response ("Your address is " <> show address))
                    LedgerSyncRes ledg ->
                        trace
                            ("TRACE [LedgerSyncRes] dddss tId" <> show tId <>
                             "ledger " <>
                             show ledg)
                            (ledger, send ledg)
                    NoAction message -> (ledger, send $ response message)
        _ <- (try ioAction) >>= ssss2
        -- _ <- takeMVar ledgerState
        -- _ <- putMVar ledgerState ledgerUpd
        _ <- modifyMVar_ ledgerState (\_ -> pure ledgerUpd)
        nextStep (BS.unpack input) loop

nextStep :: String -> IO () -> IO ()
nextStep "" io = putStrLn "Closed by peer!" -- *> io
nextStep _ io = io

www :: CLIArguments -> IO ()
www (CLIArguments {..}) = do
    ledgerState <- newMVar emptyLedger
    listenUnixSocket "sockets" id (dddds (LedgerState ledgerState) id)
