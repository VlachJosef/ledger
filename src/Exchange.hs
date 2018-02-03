{-# LANGUAGE DeriveGeneric #-}

module Exchange where

import Block
import Control.Concurrent
import Crypto.Sign.Ed25519 (Signature)
import Data.Binary
import Data.ByteString (ByteString)
import Data.List
import Data.List.NonEmpty
import Data.Semigroup
import qualified GHC.Generics as G
import Serokell.Communication.IPC
import Transaction

--import Utils
data Exchange
    = NExchange NodeExchange
    | CExchange ClientNodeExchange
    deriving (Show, G.Generic)

instance Binary Exchange

data NodeExchange
    = AddTransaction Transaction
    | QueryBlock Int
    | AddBlock Block
    deriving (Show, G.Generic)

-- instance Show NodeExchange where
--     show (AddTransaction transaction) = "AddTransaction " <> show transaction
--     show (QueryBlock n) = "QuesryBlock " <> show n
--     show (AddBlock b) = "AddBlock " <> show b
instance Binary NodeExchange

data ClientNodeExchange
    = MakeTransfer Transfer
                   Signature
    | AskBalance Address
    | Query TransactionId
    | FetchStatus
    | Register Address
    deriving (G.Generic)

instance Show ClientNodeExchange where
    show (MakeTransfer transfer signature) =
        "MakeTransfer transfer: " <> show transfer
    show (AskBalance address) = "AskBalance address: " <> show address
    show (Query txId) = "Query txId: " <> show txId
    show FetchStatus = "FetchStatus"
    show (Register address) = "Register address: " <> show address

instance Binary ClientNodeExchange

data NodeInfo = NodeInfo
    { nId :: Int
    , txPoolCount :: Int
    , blockCount :: Int
    , neighbourNodes :: [Int]
    , blocksInfo :: NonEmpty String
    , ledger :: String
    } deriving (Show, G.Generic)

instance Binary NodeInfo

data ExchangeResponse
    = NExchangeResp Int
    | SubmitResp (Maybe TransactionId)
    | BalanceResp Int
    | QueryResp Bool
    | StringResp String
    | StatusInfo NodeInfo
    | BlockResponse (Maybe Block)
    deriving (Show, G.Generic)

instance Binary ExchangeResponse

data StateAction
    = AddTransactionToNode Transaction
    | NoAction

data Broadcast
    = TxBroadcast Transaction
    | BlockBroadcast Block
