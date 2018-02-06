{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Ledger where

import Address
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup
import GHC.Generics

data LedgerError
    = AddressNotFound Address
    | InsufficientBalance Address
                          Address
                          Balance
    | NotEnoughBalance Address
                       Balance
    deriving (Eq)

instance Show LedgerError where
    show (AddressNotFound address) = "Addres not found: " <> show address
    show (InsufficientBalance fromAddress toAddress amount) =
        "Insufficient balance to transfer " <> show amount <> " from " <>
        show fromAddress <>
        " to " <>
        show toAddress
    show (NotEnoughBalance address balance) =
        "Not enough balance to transfer " <> show balance <> " from " <>
        show address

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
