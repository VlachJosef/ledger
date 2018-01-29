module Utils
    ( encodePublicKey
    ) where

import Crypto.Sign.Ed25519
import Data.ByteString (ByteString)
import Data.ByteString.Base58
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA

hash :: ByteString -> ByteString
hash = BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict

encodePublicKey :: PublicKey -> ByteString
encodePublicKey (PublicKey pk) = encodeBase58 bitcoinAlphabet (hash pk)
