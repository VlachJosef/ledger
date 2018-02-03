{-# LANGUAGE DeriveGeneric #-}

module Block where

import Crypto.Sign.Ed25519
import Data.Binary
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as G
import Time
import Transaction
import Utils

type Index = Int

data Block = Block
    { index :: Index
    , transactions :: [Transaction]
    , timestamp :: Timestamp
    } deriving (Eq, Show, G.Generic)

instance Binary Block

type BlockChain = NEL.NonEmpty Block

genesisBlock :: Block
genesisBlock = Block 1 [genesisTransaction] 0

genesisTransaction :: Transaction
genesisTransaction =
    let (transfer, signature) = genesisTransfer
        transactionId = TransactionId $ hashSignature signature
    in Transaction transactionId transfer signature 0

nodeKeyPair :: (PublicKey, SecretKey)
nodeKeyPair =
    case createKeypairFromSeed_ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" of
        Nothing -> error "seed has incorrect length"
        (Just (pk, sk)) -> (pk, sk)

genesisValue :: Int
genesisValue = 100 * 1000

genesisTransfer :: (Transfer, Signature)
genesisTransfer =
    let (pk, sk) = nodeKeyPair
        transfer = Transfer pk ((Address . encodePublicKey) pk) genesisValue
        signature = signTransfer sk transfer
    in (transfer, signature)

genesisLedger :: Map.Map Address Int
genesisLedger =
    let (pk, sk) = nodeKeyPair
        address = (Address . encodePublicKey) pk
    in Map.insert address (100 * 1000) Map.empty
