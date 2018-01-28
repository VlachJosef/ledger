{-# LANGUAGE RecordWildCards #-}

module Main where

import Client
import ClientCommandLine
import Control.Concurrent
import Crypto.Sign.Ed25519
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import Data.Monoid
import Debug.Trace
import Options.Applicative
import Serokell.Communication.IPC
import System.Directory (doesFileExist)

main :: IO ()
main = handleClient =<< execParser parseArguments

handleClient :: ClientConfig -> IO ()
handleClient clientConfig = do
    sk <-
        case keyPairDir clientConfig of
            Just filePath -> do
                mayhe <- readSecretKey filePath
                case mayhe of
                    Nothing -> error $ "No secret key on found in " <> filePath
                    Just sk -> pure sk
            Nothing -> do
                (pk, sk) <- createKeypair
                let keyName = hash2 sk
                putStrLn $ "Writing key " <> keyName
                writeSecretKey ("keys/" <> keyName) sk
                pure sk
    connectToNode clientConfig sk

hash2 :: SecretKey -> String
hash2 = SHA.showDigest . SHA.sha256 . BL.fromStrict . unSecretKey

readSecretKey :: FilePath -> IO (Maybe SecretKey)
readSecretKey filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            keysBS <- BS.readFile filePath
            pure $ Just (SecretKey keysBS)
        else pure Nothing

writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey filePath sk = do
    BS.writeFile filePath (unSecretKey sk)

connectToNode :: ClientConfig -> SecretKey -> IO ()
connectToNode clientConfig sk =
    trace
        "connectToNode CALLED"
        (connectToUnixSocket
             "sockets"
             (nodeId clientConfig)
             (connectNode clientConfig sk))

connectNode :: ClientConfig -> SecretKey -> Conversation -> IO ()
connectNode clientConfig sk conversation = do
    putStrLn
        ("Client id " <> (show . unNodeId . clientId) clientConfig <>
         ". Connected to node id " <>
         (show . unNodeId . nodeId) clientConfig)
    connectClient (NodeConversation conversation) clientConfig sk
