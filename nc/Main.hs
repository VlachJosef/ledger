{-# LANGUAGE RecordWildCards #-}
module Main where

import NcCommandLine (parseArguments, NcConfig(..))
import Serokell.Communication.IPC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Options.Applicative (execParser)
import Control.Exception (catch, IOException)
import Data.Semigroup ((<>))

main :: IO ()
main = connect =<< execParser parseArguments

connect :: NcConfig -> IO ()
connect NcConfig{..} = catch (connectToUnixSocket "sockets" clientId connectionHandler) resolve

resolve :: IOException -> IO ()
resolve ex = putStrLn $ "Ups: " <> show ex

connectionHandler :: Conversation -> IO ()
connectionHandler Conversation {..} = do
  command  <- getLine
  response <- send (BSC.pack command) *> recvAll recv
  putStrLn (BSC.unpack response)

recvAll :: IO ByteString -> IO ByteString
recvAll recv = loop "" where
  loop :: ByteString -> IO ByteString
  loop acc = do
    d <- recv
    if BSC.length d == 4096
      then loop (acc <> d)
      else pure $ acc <> d
