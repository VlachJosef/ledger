module Main where

import           Address
import           Client
import           ClientCommandLine
import           Control.Exception          (IOException, catch)
import           Control.Logging
import           Crypto.Sign.Ed25519
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Digest.Pure.SHA       as SHA
import           Data.Functor
import           Data.Monoid
import           Options.Applicative
import           Serokell.Communication.IPC
import           System.Directory           (doesFileExist, removeFile)
import           System.Posix.Signals
import           Time.Units                 (sec, threadDelay)
import           Utils

main :: IO ()
main = handleClient =<< execParser parseArguments

terminationHandler :: ClientConfig -> Signal ->  Handler
terminationHandler clientConfig signal = CatchOnce $ do
    logThread $ "Caught " <> show signal <> " signal."
    let socketFile = "sockets/" <> (show . unNodeId . clientId) clientConfig <> ".sock"
    fileExists <- doesFileExist socketFile
    if fileExists
      then do
        logThread $ "Removing socket file: " <> socketFile
        removeFile socketFile
      else logThread $ "File: " <> socketFile <> " not found."
    flushLog
    raiseSignal signal

handleClient :: ClientConfig -> IO ()
handleClient clientConfig = do
  void $ installHandler sigTERM (terminationHandler clientConfig sigTERM) Nothing
  void $ installHandler sigINT  (terminationHandler clientConfig sigINT)  Nothing

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
connectToNode clientConfig sk = loop where
  loop = catch connect resolve

  resolve :: IOException -> IO ()
  resolve ex = do
    putStrLn $ "Client error: " <> show ex <> ", will retry in " <> show retryTimeout
    threadDelay retryTimeout
    putStrLn $ "Retrying connection to " <> (show . nodeId) clientConfig <> " node."
    loop

  connect = connectToUnixSocket "sockets" (nodeId clientConfig) (connectNode clientConfig sk)

  retryTimeout = sec 1

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
