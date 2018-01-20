{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( www
    ) where

import Control.Concurrent (forkIO)
import Control.Monad
import Control.Newtype
import Crypto.Sign.Ed25519
import Data.Binary (Binary)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Conversion as DBC
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
    }

newtype TxId = TxId
    { unTxId :: Int
    } deriving (Eq, Ord, Binary)

newtype Address = Address
    { rawAddress :: S.ByteString
    } deriving (Show, Read, Eq, Ord, Monoid)

type Balance = Int

newtype Example =
    Example Int
    deriving (Generic)

instance Newtype Example

newtype Ledger = Ledger
    { unLedger :: Map Address Balance
    } deriving (Eq, Show, Monoid, Generic)

instance Newtype Ledger

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
    | InvalidCommand S.ByteString

data CommandResult
    = LedgerUpdate Ledger
    | Registration Address
                   Ledger
    | BalanceInfo Balance
    | NoAction String

processCommand :: Ledger -> Command -> CommandResult
processCommand ledger (Submit (Transaction {..})) =
    either
        (\a -> NoAction (show a))
        (LedgerUpdate)
        (transferBalance from to amount ledger)
processCommand ledger (Query (TxId txId)) = NoAction ""
processCommand ledger (Balance address) =
    either (\a -> NoAction (show a)) (BalanceInfo) (balance ledger address)
processCommand ledger (Register pk) = Registration address ledgerUpd
  where
    (address, ledgerUpd) = register ledger pk
processCommand ledger (InvalidCommand command) = NoAction (show command)

stringToCommand :: [S.ByteString] -> IO Command
stringToCommand ["register"] = do
    (pk, sk) <- createKeypair
    pure $ Register pk
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
        then Left $ NotEnoughBalance address amount
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

runNode :: (Net.Socket -> Conversation -> IO Bool) -> IO ()
runNode = listenUnixSocket "sockets" (NodeId 0)

dddds :: Net.Socket -> Conversation -> IO Bool
dddds socket (Conversation {..}) = True <$ forkIO (loop emptyLedger)
  where
    loop :: Ledger -> IO ()
    loop ledger = do
        input <- recv
        command <- stringToCommand $ BS.words input
        let (ledgerUpd, ioAction) =
                case processCommand ledger command of
                    LedgerUpdate ledger -> (ledger, pure ())
                    BalanceInfo balance ->
                        (ledger, putStrLn ("Your balance is " <> show balance))
                    Registration address ledger ->
                        (ledger, putStrLn ("Your address is " <> show address))
                    NoAction message -> (ledger, putStrLn message)
        _ <- ioAction
        nextStep (BS.unpack input) (loop ledgerUpd)

nextStep :: String -> IO () -> IO ()
nextStep "" _ = putStrLn "Closed by peer!"
nextStep _ io = io

www :: IO ()
www = runNode dddds
