{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Client
    ( connectClient
    , NodeConversation(..)
    ) where

import Block as B
import ClientCommandLine
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Newtype
import Crypto.Sign.Ed25519
import Data.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Conversion as DBC
import Data.ByteString.Conversion.To
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import Data.Int
import Data.Map (Map)
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Exchange
import GHC.Generics
import NodeCommandLine
import Text.Read
import Transaction

import qualified Data.Map as Map
import Data.Semigroup
import Serokell.Communication.IPC

import qualified Network.Socket as Net

data PossibleCmd
    = Cmd ClientCmd
    | UnknownCmd
    | ErrorCmd String

data ClientCmd
    = StatusCmd
    | TransferCmd Address
                  Int

data ClientReq =
    StatusReq

convertToInt :: S.ByteString -> Maybe Int
convertToInt = DBC.fromByteString

toClientCmd :: S.ByteString -> PossibleCmd
toClientCmd bs =
    case BS.words bs of
        ["status"] -> Cmd StatusCmd
        ["SUBMIT", address, amount] ->
            case convertToInt amount of
                Nothing ->
                    ErrorCmd
                        ("Amount must be a number, got: " <> (BS.unpack amount))
                (Just n) -> Cmd $ TransferCmd (Address address) n
        unknown -> UnknownCmd

nodeCommunication :: SecretKey
                  -> NodeConversation
                  -> ClientCmd
                  -> IO ExchangeResponse
nodeCommunication sk (NodeConversation Conversation {..}) clientCmd =
    case clientCmd of
        StatusCmd -> do
            response <- send "status" *> recv
            pure $ decode (BL.fromStrict response)
        TransferCmd address amount -> do
            let transfer = Transfer (toPublicKey sk) address amount
            let transferSignature = dsign sk (encodeTransfer transfer)
            let payload = CExchange $ MakeTransfer transfer transferSignature
            let encodedTransfer = BL.toStrict (encode payload)
            response <- send encodedTransfer *> recv
            pure $ decode (BL.fromStrict response)

connect :: SecretKey -> NodeConversation -> Conversation -> IO Bool
connect sk nc (Conversation {..}) = do
    True <$
        forkIO
            ((do tId <- myThreadId
                 (putStrLn $ "CLIENT FORKING from " <> show tId)) <* do loop)
  where
    comm = nodeCommunication sk nc
    loop :: IO ()
    loop = do
        tId <- myThreadId
        input <- recv
        case toClientCmd input of
            Cmd clientCmd -> do
                exchangeResp <- comm clientCmd
                case exchangeResp of
                    (NExchangeResp x) -> send $ response (show x)
            ErrorCmd error -> send $ response error
            UnknownCmd -> send $ response "Unkown command"
        putStrLn $ "Client command received " <> show tId
        nextStep (BS.unpack input) loop

nextStep :: String -> IO () -> IO ()
nextStep "" io = putStrLn "Closed by peer!"
nextStep _ io = io

response :: String -> S.ByteString
response str = BL.toStrict (toByteString (str <> "\n"))

connectClient :: NodeConversation -> ClientConfig -> SecretKey -> IO ()
connectClient nc (ClientConfig {..}) sk = do
    listenUnixSocket "sockets" nodeId (connect sk nc)

newtype NodeConversation = NodeConversation
    { unNodeConverasation :: Conversation
    }
