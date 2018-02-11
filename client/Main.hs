module Main where

import Client
import ClientCommandLine
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
    sk <-
        case keyPairDir clientConfig of
            Just filePath -> do
                maybeSk <- readSecretKey filePath
                case maybeSk of
                    Nothing -> error $ "No secret key on found in " <> filePath
                    Just sk -> pure sk
            Nothing -> do
                (_, sk) <- createKeypair
                let keyName = hash sk
                logThread $ "Writing key " <> keyName
                writeSecretKey ("keys/" <> keyName) sk
                pure sk
    connectToNode clientConfig sk

hash :: SecretKey -> String
hash = SHA.showDigest . SHA.sha256 . BL.fromStrict . unSecretKey

readSecretKey :: FilePath -> IO (Maybe SecretKey)
readSecretKey filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            keysBS <- BS.readFile filePath
            pure $ Just (SecretKey keysBS)
        else pure Nothing

writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey filePath sk = BS.writeFile filePath (unSecretKey sk)

connectToNode :: ClientConfig -> SecretKey -> IO ()
connectToNode clientConfig sk =
    connectToUnixSocket
        "sockets"
        (nodeId clientConfig)
        (connectNode clientConfig sk)

connectNode :: ClientConfig -> SecretKey -> Conversation -> IO ()
connectNode clientConfig sk conversation = do
    let nc = NodeConversation conversation
    logThread $ "Client id " <> (showNodeId . clientId) clientConfig <> ". Connected to node id " <> (showNodeId . nodeId) clientConfig
    registerResp <- register sk nc
    logThread $ "Registration status: " <> (show registerResp)
    connectClient nc clientConfig sk
