{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Transaction where

import           Address
import           Crypto.Sign.Ed25519
import           Data.Binary
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BL
import           Data.Semigroup
import qualified GHC.Generics          as G
import           Time                  (Timestamp)

data Transfer = Transfer
    { from   :: PublicKey
    , to     :: Address
    , amount :: Int
    } deriving (Eq, G.Generic)

data InitiateTransfer = InitiateTransfer
    { itFrom   :: SecretKey
    , itTo     :: PublicKey
    , itAmount :: Int
    } deriving Eq

instance Show Transfer where
    show transfer =
           "from "     <> (show . deriveAddress . from) transfer
        <> ", to "     <> (show . to) transfer
        <> ", amount " <> (show . amount) transfer

instance Show InitiateTransfer where
    show InitiateTransfer{..} =
           "from "     <> show itFrom
        <> ", to "     <> show itTo
        <> ", amount " <> show itAmount

newtype TransactionId = TransactionId
    { unTransactionId :: ByteString
    } deriving (Eq, G.Generic)

instance Show TransactionId where
    show = BSC.unpack . unTransactionId

data Transaction = Transaction
    { transactionId :: TransactionId
    , transfer      :: Transfer
    , signature     :: Signature
    , timestamp     :: Timestamp
    } deriving (Eq, G.Generic)

instance Show Transaction where
    show transaction =
           "transactionId: " <> (show . transactionId) transaction
        <> "\ntransfer: "    <> (show . transfer) transaction
        <> "\ntimestamp: "   <> (show . Transaction.timestamp) transaction

data TransactionError =
    InvalidSignature
    deriving (Show)

instance Binary PublicKey

instance Binary Signature

instance Binary Transfer

instance Binary TransactionId

instance Binary Transaction

encodeTransfer :: Transfer -> ByteString
encodeTransfer transfer = BL.toStrict (encode transfer)

signTransfer :: SecretKey -> Transfer -> Signature
signTransfer sk tx = dsign sk (encodeTransfer tx)

verifyTransfer :: Signature -> Transfer -> Bool
verifyTransfer signature transfer =
    let pk = from transfer
        enc = encodeTransfer transfer
    in dverify pk enc signature

verifyTx :: Transaction -> Either TransactionError ()
verifyTx Transaction {..} =
    let pk = from transfer
        enc = encodeTransfer transfer
    in if dverify pk enc signature
           then Right ()
           else Left InvalidSignature
