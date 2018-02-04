{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Address
    ( Address(..)
    , deriveAddress
    ) where

import Crypto.Sign.Ed25519
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Utils

newtype Address = Address
    { rawAddress :: ByteString
    } deriving (Read, Eq, Ord, Binary)

instance Show Address where
    show = BS.unpack . rawAddress

deriveAddress :: PublicKey -> Address
deriveAddress = Address . encodePublicKey
