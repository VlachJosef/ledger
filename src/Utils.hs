module Utils
    ( encodePublicKey
    , decodePublicKey
    , encodeSignature
    , now
    , logThread
    , showNodeId
    , recvAll
    , hash
    ) where

import Control.Concurrent (myThreadId)
import Control.Logging
import Crypto.Sign.Ed25519
import Data.Semigroup
import Data.ByteString (ByteString)
import Data.ByteString.Base58
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding (log)
import Serokell.Communication.IPC (NodeId, unNodeId)
import Time

encodeSignature :: Signature -> ByteString
encodeSignature = hash . unSignature

encodePublicKey :: PublicKey -> ByteString
encodePublicKey = Base16.encode . unPublicKey

decodePublicKey :: ByteString -> PublicKey
decodePublicKey = PublicKey . fst .Base16.decode

hash :: ByteString -> ByteString
hash = Base16.encode . MD5.hash

now :: IO Timestamp
now = round . (* 1000000) <$> getPOSIXTime

logThread :: String -> IO ()
logThread msg = do
    tId <- myThreadId
    log $ T.pack $ "[" <> show tId <> "] " <> msg

showNodeId :: NodeId -> String
showNodeId = show . unNodeId

recvAll :: IO ByteString -> IO ByteString
recvAll recv = loop "" where
  loop :: ByteString -> IO ByteString
  loop acc = do
    d <- recv
    if BSC.length d == 4096
      then loop (acc <> d)
      else do
        let resp = acc <> d
        resp <$ logThread ("Recieved " <> show (BSC.length resp) <> " bytes of data.")
