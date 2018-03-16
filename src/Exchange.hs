{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Exchange where

import GHC.Int
import Address
import Block
import Crypto.Sign.Ed25519 (unSecretKey, unPublicKey, SecretKey(..), PublicKey(..))
import Data.Binary
import Data.Binary.Get (ByteOffset, Get, getByteString, getWord8, getInt32be, runGet)
import Data.Binary.Put (                 putByteString, putWord8, putInt32be, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty
import Data.Semigroup
import Debug.Trace
import qualified GHC.Generics as G
import OrphanedShow
import Serokell.Communication.IPC (NodeId(..))
import Transaction

data Exchange
    = NodeExchange NodeId NodeExchange
    | ClientExchange ClientExchange
    | ClientExchangeCLI ClientExchangeCLI
    deriving (Show, G.Generic)

instance Binary Exchange where
  put (ClientExchange (MakeTransfer InitiateTransfer{..})) = do
    putWord8 0
    putByteString . unSecretKey $ itFrom
    putByteString . unPublicKey $ itTo
    putInt32be $ fromIntegral itAmount
  put (ClientExchange (Query txId)) = do
    putWord8 1
    putByteString . unTransactionId $ txId
  put (ClientExchange (AskBalance pk)) = do
    putWord8 2
    putByteString . unPublicKey $ pk
  put (ClientExchangeCLI FetchStatus) =
    putWord8 3
  put (ClientExchangeCLI (AskBalanceByAddress address)) =  do
    putWord8 4
    putByteString . rawAddress $ address
  put (NodeExchange nodeId nodeExchange) = do
    putWord8 5
    put nodeId
    put nodeExchange

  get = do
    determinant <- getWord8

    case determinant of
      0 -> do
        sk     <- SecretKey <$> getByteString 64
        pk     <- PublicKey <$> getByteString 32
        amount <- getInt32be
        pure . ClientExchange . MakeTransfer $ InitiateTransfer sk pk (fromIntegral amount)
      1 -> do
        txId <- TransactionId <$> getByteString 32
        pure . ClientExchange . Query $ txId
      2 -> do
        pk <- PublicKey <$> getByteString 32
        pure . ClientExchange . AskBalance $ pk
      3 -> pure . ClientExchangeCLI $ FetchStatus
      4 -> do
        address <- Address <$> getByteString 32
        pure . ClientExchangeCLI . AskBalanceByAddress $ address
      5 -> do
        nodeId <- get :: (Get NodeId)
        nodeExchange <- get :: (Get NodeExchange)
        pure $ NodeExchange nodeId nodeExchange
      _ -> error "Unknown data received"

data NodeExchange
    = AddTransaction Transaction
    | QueryBlock Index
    | AddBlock Block
    deriving (Show, G.Generic)

instance Binary NodeExchange

data ClientExchange
    = MakeTransfer InitiateTransfer
    | AskBalance PublicKey
    | Query TransactionId
    deriving G.Generic

data ClientExchangeCLI
    = AskBalanceByAddress Address
    | FetchStatus
    deriving G.Generic

instance Binary ClientExchangeCLI

instance Show ClientExchange where
    show (MakeTransfer tran)           = "MakeTransfer transfer: " <> show tran
    show (AskBalance pk)               = "AskBalance pk: " <> show pk
    show (Query txId)                  = "Query txId: " <> show txId

instance Show ClientExchangeCLI where
    show (AskBalanceByAddress address) = "AskBalance address: " <> show address
    show FetchStatus                   = "FetchStatus"

data NodeInfo = NodeInfo
    { nId            :: Int
    , txPoolCount    :: Int
    , blockCount     :: Int
    , neighbourNodes :: [Int]
    , blocksInfo     :: NonEmpty String
    , ledger         :: String
    } deriving (Show, G.Generic)

instance Binary NodeInfo

data NodeExchangeResponse
    = BlockResponse (Maybe Block)
    | NodeNoResponse
    deriving (Show, G.Generic)

data ClientExchangeResponse
    = SubmitResp (Maybe TransactionId)
    | BalanceResp Int
    | QueryResp Bool
    deriving (Show, G.Generic)

data ClientExchangeCLIResponse
    = BalanceRespCLI Int
    | StatusInfo NodeInfo
    deriving (Show, G.Generic)

instance Binary NodeExchangeResponse
instance Binary ClientExchangeCLIResponse

data Broadcast
    = TxBroadcast Transaction
    | BlockBroadcast Block deriving (Show)

type DecodeRes a = Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, a)

decodeExchangeAs
    :: Binary a
    => ByteString -> DecodeRes a
decodeExchangeAs = decodeOrFail . BL.fromStrict

runGetStrict :: Get a -> ByteString -> a
runGetStrict a = runGet a . BL.fromStrict

runPutStrict :: Put -> ByteString
runPutStrict = BL.toStrict . runPut

decodeSubmitResp :: ClientExchangeResponse -> Put
decodeSubmitResp (SubmitResp Nothing) = putWord8 0
decodeSubmitResp (SubmitResp (Just transactionId)) = do
  putWord8 1
  putByteString . unTransactionId $ transactionId
decodeSubmitResp (BalanceResp bal) = put bal
decodeSubmitResp (QueryResp exists) = if exists then putWord8 1 else putWord8 0

encodeStringResp :: String -> Put
encodeStringResp str = do
  putWord8 $ fromIntegral (Prelude.length str)
  putByteString $ BSC.pack str

encodeMakeTransferResp :: Get ClientExchangeResponse
encodeMakeTransferResp = do
  isSuccess <- getWord8
  case isSuccess of
    0 -> pure $ SubmitResp Nothing
    1 -> do
      txId <- getByteString 32
      pure $ (SubmitResp . Just . TransactionId) txId
    _ -> error "Ups, Transfer"

encodeBalanceResp :: Get ClientExchangeResponse
encodeBalanceResp = do
  balance <- get :: Get Int
  pure $ BalanceResp balance

encodeQueryResp :: Get ClientExchangeResponse
encodeQueryResp = do
  bool <- getWord8
  case bool of
    0 -> pure $ QueryResp False
    1 -> pure $ QueryResp True
    _ -> error "Ups, Query"

decodeExchange :: ByteString -> DecodeRes Exchange
decodeExchange = decodeExchangeAs

decodeNodeExchangeResponse :: ByteString -> DecodeRes NodeExchangeResponse
decodeNodeExchangeResponse = decodeExchangeAs

decodeClientExchangeCLIResponse :: ByteString -> DecodeRes ClientExchangeCLIResponse
decodeClientExchangeCLIResponse = decodeExchangeAs
