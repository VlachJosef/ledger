{-# LANGUAGE DeriveGeneric #-}

module Block where

import Data.Binary
import qualified GHC.Generics as G
import Time
import Transaction

type Index = Int

data Block = Block
    { index :: Index
    , transactions :: [Transaction]
    , timestamp :: Timestamp
    } deriving (Eq, Show, G.Generic)

instance Binary Block

type BlockChain = [Block]
