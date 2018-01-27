{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transaction where

import Crypto.Sign.Ed25519
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GHC.Generics as G
import Time

newtype Address = Address
    { rawAddress :: ByteString
    } deriving (Show, Read, Eq, Ord, Monoid, Binary)

data Transfer = Transfer
    { from :: PublicKey
    , to :: Address
    , amount :: Int
    } deriving (Eq, Show, G.Generic)

data Transaction = Transaction
    { transfer :: Transfer
    , signature :: Signature
    , timestamp :: Timestamp
    } deriving (Eq, Show, G.Generic)

instance Binary PublicKey

instance Binary Signature

instance Binary Transfer

instance Binary Transaction

mkTransaction :: Transfer -> Signature -> IO Transaction
mkTransaction transfer signature = do
    timestamp <- now
    pure $ Transaction transfer signature timestamp

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

now :: IO Timestamp
now = round <$> (* 1000000) <$> getPOSIXTime

text :: (PublicKey -> Address) -> IO Transaction
text f = do
    (pk, sk) <- createKeypair
    let toAddress = f pk
    (pk2, sk2) <- createKeypair
    let transfer = Transfer pk2 toAddress 100
    let sign1 = signTransfer sk transfer
    let tx1 = Transaction transfer sign1 0
    pure tx1
