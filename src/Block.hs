{-# LANGUAGE DeriveGeneric #-}

module Block where

import Crypto.Sign.Ed25519
import Data.Binary
import qualified Data.List.NonEmpty as NEL
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
    in Transaction transfer signature 0

nodeKeyPair :: (PublicKey, SecretKey)
nodeKeyPair =
    case createKeypairFromSeed_ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" of
        Nothing -> error "seed has incorrect length"
        (Just (pk, sk)) -> (pk, sk)

genesisTransfer :: (Transfer, Signature)
genesisTransfer =
    let (pk, sk) = nodeKeyPair
        transfer = Transfer pk ((Address . encodePublicKey) pk) (100 * 1000)
        signature = signTransfer sk transfer
    in (transfer, signature)
