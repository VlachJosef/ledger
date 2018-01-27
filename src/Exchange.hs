{-# LANGUAGE DeriveGeneric #-}

module Exchange where

import Block
import Control.Concurrent
import Crypto.Sign.Ed25519 (Signature)
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified GHC.Generics as G
import Serokell.Communication.IPC
import Transaction

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

instance Binary NodeExchange

data ClientNodeExchange
    = MakeTransfer Transfer
                   Signature
    | AskBalance Address
    deriving (Show, G.Generic)

instance Binary ClientNodeExchange

data ExchangeResponse =
    NExchangeResp Int
    deriving (Show, G.Generic)

instance Binary ExchangeResponse

data StateAction
    = AddTransactionToNode Transaction
    | NoAction
