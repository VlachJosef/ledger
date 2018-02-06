{-# LANGUAGE DeriveGeneric #-}

module Exchange where

import Address
import Block
import Crypto.Sign.Ed25519 (Signature)
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty
import Data.Semigroup
import qualified GHC.Generics as G
import Transaction

data Exchange
    = NodeExchange NodeExchange
    | ClientExchange ClientExchange
    deriving (Show, G.Generic)

instance Binary Exchange

data NodeExchange
    = AddTransaction Transaction
    | QueryBlock Int
    | AddBlock Block
    deriving (Show, G.Generic)

instance Binary NodeExchange

data ClientExchange
    = MakeTransfer Transfer
                   Signature
    | AskBalance Address
    | Query TransactionId
    | FetchStatus
    | Register Address
    deriving (G.Generic)

instance Show ClientExchange where
    show (MakeTransfer tran _) = "MakeTransfer transfer: " <> show tran
    show (AskBalance address) = "AskBalance address: " <> show address
    show (Query txId) = "Query txId: " <> show txId
    show FetchStatus = "FetchStatus"
    show (Register address) = "Register address: " <> show address

instance Binary ClientExchange

data NodeInfo = NodeInfo
    { nId :: Int
    , txPoolCount :: Int
    , blockCount :: Int
    , neighbourNodes :: [Int]
    , blocksInfo :: NonEmpty String
    , ledger :: String
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
    | StringResp String
    | StatusInfo NodeInfo
    deriving (Show, G.Generic)

instance Binary NodeExchangeResponse

instance Binary ClientExchangeResponse

data Broadcast
    = TxBroadcast Transaction
    | BlockBroadcast Block

decodeExchangeAs
    :: Binary a
    => ByteString -> a
decodeExchangeAs = decode . BL.fromStrict

decodeExchange :: ByteString -> Exchange
decodeExchange = decodeExchangeAs

decodeNodeExchangeResponse :: ByteString -> NodeExchangeResponse
decodeNodeExchangeResponse = decodeExchangeAs

decodeClientExchangeResponse :: ByteString -> ClientExchangeResponse
decodeClientExchangeResponse = decodeExchangeAs
