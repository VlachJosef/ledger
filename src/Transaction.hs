{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transaction where

import Address
import Crypto.Sign.Ed25519
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup
import qualified GHC.Generics as G
import Time (Timestamp)
import Utils

data Transfer = Transfer
    { from :: PublicKey
    , to :: Address
    , amount :: Int
    } deriving (Eq, G.Generic)

instance Show Transfer where
    show transfer =
        "from " <> (show . deriveAddress . from) transfer <> ", to " <>
        (show . to) transfer <>
        ", amount " <>
        (show . amount) transfer

newtype TransactionId = TransactionId
    { unTransactionId :: ByteString
    } deriving (Eq, G.Generic)

instance Show TransactionId where
    show = BS.unpack . unTransactionId

data Transaction = Transaction
    { transactionId :: TransactionId
    , transfer :: Transfer
    , signature :: Signature
    , timestamp :: Timestamp
    } deriving (Eq, G.Generic)

instance Show Transaction where
    show transaction =
        "transactionId: " <> (show . transactionId) transaction <> "\n" <>
        "transfer: " <>
        (show . transfer) transaction <>
        "\n" <>
        "timestamp: " <>
        (show . Transaction.timestamp) transaction

instance Binary PublicKey

instance Binary Signature

instance Binary Transfer

instance Binary TransactionId

instance Binary Transaction

encodeTransfer :: Transfer -> ByteString
encodeTransfer transfer = BL.toStrict (encode transfer)

signTransfer :: SecretKey -> Transfer -> Signature
signTransfer sk tx = dsign sk (encodeTransfer tx)

data TransactionError =
    InvalidSignature
    deriving (Show)

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
           then (Right ())
           else (Left InvalidSignature)
-- text :: (PublicKey -> Address) -> IO Transaction
-- text f = do
--     (pk, sk) <- createKeypair
--     let toAddress = f pk
--     (pk2, sk2) <- createKeypair
--     let transfer = Transfer pk2 toAddress 100
--     let sign1 = signTransfer sk transfer
--     let tx1 = Transaction transfer sign1 0
--     pure tx1
