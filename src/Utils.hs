module Utils
    ( encodePublicKey
    , encodeSignature
    , now
    , logThread
    , showNodeId
    , nextStep
    ) where

import Control.Concurrent (myThreadId)
import Control.Logging
import Crypto.Sign.Ed25519
import Data.Semigroup
import Data.ByteString (ByteString)
import Data.ByteString.Base58
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Conversion as DBC
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding (log)
import Serokell.Communication.IPC (NodeId, unNodeId)
import Time

encodeSignature :: Signature -> ByteString
encodeSignature = hash . unSignature

encodePublicKey :: PublicKey -> ByteString
encodePublicKey = Base16.encode . unPublicKey

hash :: ByteString -> ByteString
hash = encodeBase58 bitcoinAlphabet . BL.toStrict . SHA.bytestringDigest . SHA.sha256 . BL.fromStrict

now :: IO Timestamp
now = round <$> (* 1000000) <$> getPOSIXTime

logThread :: String -> IO ()
logThread msg = do
    tId <- myThreadId
    log $ T.pack $ "[" <> show tId <> "] " <> msg

showNodeId :: NodeId -> String
showNodeId = show . unNodeId

nextStep :: String -> IO () -> IO ()
nextStep "" _ = putStrLn "Closed by peer!"
nextStep _ io = io
