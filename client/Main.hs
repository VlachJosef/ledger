module Main where

import Address
import Client
import ClientCommandLine
import Control.Logging
import Crypto.Sign.Ed25519
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import Data.Monoid
import Options.Applicative
import Serokell.Communication.IPC
import System.Directory (doesFileExist)
import Utils

main :: IO ()
main = handleClient =<< execParser parseArguments

handleClient :: ClientConfig -> IO ()
handleClient clientConfig = do
    sk <- readSecretKey (keyPairDir clientConfig)
    connectToNode clientConfig sk

hash :: SecretKey -> String
hash = SHA.showDigest . SHA.sha256 . BL.fromStrict . unSecretKey

readSecretKey :: FilePath -> IO SecretKey
readSecretKey filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            keysBS <- BS.readFile filePath
            pure $ SecretKey keysBS
        else error $ "No secret key on found in " <> filePath

connectToNode :: ClientConfig -> SecretKey -> IO ()
connectToNode clientConfig sk =
    connectToUnixSocket
        "sockets"
        (nodeId clientConfig)
        (connectNode clientConfig sk)

connectNode :: ClientConfig -> SecretKey -> Conversation -> IO ()
connectNode clientConfig sk conversation = let
  nc = NodeConversation conversation
  cId = (showNodeId . clientId) clientConfig
  nId = (showNodeId . nodeId) clientConfig
  address = (show . deriveAddress . toPublicKey) sk
  in withFileLogging ("log/client-" <> cId <> ".log") $ do
    logThread $ "Client id " <> cId <> ", address: " <> address
    logThread $ "Client id " <> cId <> ". Connected to node id " <> nId
    connectClient nc clientConfig sk
