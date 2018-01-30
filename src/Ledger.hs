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

-- type MBalance = MVar Balance
-- newtype Ledger =
--     Ledger (Map Address MBalance)
--     deriving (Eq, Generic)
newtype Ledger =
    Ledger (Map Address Balance)
    deriving (Eq, Generic, Show)

newtype LedgerState =
    LedgerState (MVar Ledger)
    deriving (Eq, Generic)

-- freeze :: Ledger -> IO LedgerFreeze
-- freeze (Ledger ledger) = LedgerFreeze <$> traverse readMVar ledger
-- unFreeze :: LedgerFreeze -> IO Ledger
-- unFreeze (LedgerFreeze ledgerFreeze) = Ledger <$> traverse newMVar ledgerFreeze
instance Newtype Ledger--instance Binary LedgerFreeze
-- emptyLedger :: Ledger
-- emptyLedger = Ledger Map.empty
