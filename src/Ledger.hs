{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Ledger where

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
import qualified Data.Map.Strict as MAP
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Exchange
import GHC.Generics
import Text.Read
import Transaction

import qualified Data.Map.Strict as Map
import Data.Semigroup
import Serokell.Communication.IPC

import qualified Network.Socket as Net

data LedgerError
    = AddressNotFound Address
    | InsufficientBalance Address
    | NotEnoughBalance Address
                       Balance
    deriving (Eq, Show)

newtype TxId = TxId
    { unTxId :: Int
    } deriving (Eq, Ord, Show, Binary)

type Balance = Int

newtype Ledger =
    Ledger (Map Address Balance)
    deriving (Eq, Generic)

instance Show Ledger where
    show (Ledger ledger) =
        Map.foldrWithKey
            (\address balance acc ->
                 acc <> show address <> " " <> show balance <> "\n")
            ""
            ledger

newtype LedgerState =
    LedgerState (MVar Ledger)
    deriving (Eq, Generic)

instance Newtype Ledger
