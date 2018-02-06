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
    { index :: Int
    , transactions :: [Transaction]
    , timestamp :: Timestamp
    } deriving (Eq, Show, G.Generic)

instance Binary Block

type BlockChain = NEL.NonEmpty Block

genesisBlock :: Block
genesisBlock = Block 1 [genesisTransaction] 0

genesisTransaction :: Transaction
genesisTransaction =
    let (tran, sig) = genesisTransfer
        txId = TransactionId $ encodeSignature sig
    in Transaction txId tran sig 0

nodeKeyPair :: (PublicKey, SecretKey)
nodeKeyPair =
    case createKeypairFromSeed_ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" of
        Nothing -> error "seed has incorrect length"
        Just (pk, sk) -> (pk, sk)

genesisValue :: Int
genesisValue = 100 * 1000

genesisTransfer :: (Transfer, Signature)
genesisTransfer =
    let (pk, sk) = nodeKeyPair
        tran = Transfer pk (deriveAddress pk) genesisValue
        sig = signTransfer sk tran
    in (tran, sig)

genesisLedger :: Map.Map Address Int
genesisLedger =
    let pk = fst nodeKeyPair
        address = deriveAddress pk
    in Map.insert address genesisValue Map.empty
