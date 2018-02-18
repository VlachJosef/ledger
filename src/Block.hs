{-# LANGUAGE DeriveGeneric #-}

module Block where

import Address
import Crypto.Sign.Ed25519
import Data.Binary
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as G
import Time
import Transaction
import Utils

data Block = Block
    { index        :: Int
    , transactions :: [Transaction]
    , timestamp    :: Timestamp
    } deriving (Eq, Show, G.Generic)

instance Binary Block

type BlockChain = NEL.NonEmpty Block

genesisBlock :: [(Address, Int)] ->  Block
genesisBlock xs = Block 1 (genesisTransactions xs) 0

genesisTransaction :: (Address, Int) -> Transaction
genesisTransaction (address, amount) =
  let (pk, sk) = nodeKeyPair
      tran     = Transfer pk address amount
      sig      = signTransfer sk tran
      txId     = TransactionId $ encodeSignature sig
  in Transaction txId tran sig 0

genesisTransactions :: [(Address, Int)] -> [Transaction]
genesisTransactions xs = genesisTransaction <$> xs

nodeKeyPair :: (PublicKey, SecretKey)
nodeKeyPair =
    case createKeypairFromSeed_ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" of
        Nothing       -> error "seed has incorrect length"
        Just (pk, sk) -> (pk, sk)
