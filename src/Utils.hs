module Utils
    ( encodePublicKey
    , encodeSignature
    , now
    ) where

import Crypto.Sign.Ed25519
import Data.ByteString (ByteString)
import Data.ByteString.Base58
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import Data.Time.Clock.POSIX (getPOSIXTime)
import Time

encodeSignature :: Signature -> ByteString
encodeSignature = hash . unSignature

encodePublicKey :: PublicKey -> ByteString
encodePublicKey = hash . unPublicKey

hash :: ByteString -> ByteString
hash =
    encodeBase58 bitcoinAlphabet .
    BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict

now :: IO Timestamp
now = round <$> (* 1000000) <$> getPOSIXTime
