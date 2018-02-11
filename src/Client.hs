{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Client
    ( connectClient
    , register
    , NodeConversation(..)
    ) where

import Address
import ClientCommandLine
import Control.Concurrent
import Crypto.Sign.Ed25519
import Data.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Conversion as DBC
import Data.ByteString.Conversion.To
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup
import Exchange
import Serokell.Communication.IPC
import Transaction
import Utils

data PossibleCmd
    = Cmd ClientCmd
    | UnknownCmd
    | ErrorCmd String
    deriving (Show)

data ClientCmd
    = StatusCmd
    | BalanceCmd Address
    | QueryCmd TransactionId
    | TransferCmd Address
                  Int
    deriving (Show)

newtype NodeConversation = NodeConversation
    { unNodeConversation :: Conversation
    }

convertToInt :: S.ByteString -> Maybe Int
convertToInt = DBC.fromByteString

toPossibleCmd :: S.ByteString -> PossibleCmd
toPossibleCmd bs =
    case BS.words bs of
        ["status"]           ->  Cmd StatusCmd
        ["BALANCE", address] -> (Cmd . BalanceCmd . Address) address
        ["QUERY", txId]      -> (Cmd . QueryCmd . TransactionId) txId
        ["SUBMIT", address, amount] ->
            case convertToInt amount of
                Nothing -> ErrorCmd ("Amount must be a number, got: " <> (BS.unpack amount))
                Just n  -> Cmd $ TransferCmd (Address address) n
        _ -> UnknownCmd

clientCmdToNodeExchange :: SecretKey -> ClientCmd -> ClientExchange
clientCmdToNodeExchange sk clientCmd =
    case clientCmd of
        StatusCmd          -> FetchStatus
        BalanceCmd address -> AskBalance address
        QueryCmd txId      -> Query txId
        TransferCmd address amount ->
            let transfer  = Transfer (toPublicKey sk) address amount
                transferSignature = dsign sk (encodeTransfer transfer)
            in MakeTransfer transfer transferSignature

register :: SecretKey -> NodeConversation -> IO ClientExchangeResponse
register sk conversation =
    let address = deriveAddress (toPublicKey sk)
        exchange = Register address
    in do sendExchange conversation exchange

sendExchange :: NodeConversation -> ClientExchange -> IO ClientExchangeResponse
sendExchange (NodeConversation Conversation {..}) exchange =
    let encodedExchange = (BL.toStrict . encode . ClientExchange) exchange
    in do resp <- send encodedExchange *> recv
          pure $ decodeClientExchangeResponse resp

connect :: NodeId -> SecretKey -> NodeConversation -> Conversation -> IO Bool
connect clientId sk nc (Conversation {..}) = do
  True <$
    forkIO ((logThread $ "Client " <> showNodeId clientId <> ". Forking new thread!") <* loop)
  where
    sendNodeExchange = sendExchange nc
    ccToNodeExchange = clientCmdToNodeExchange sk
    sendResponse = send . response
    loop :: IO ()
    loop = do
        input <- recv
        let possibleCmd = toPossibleCmd input
        logThread $ "Command received: " <> show possibleCmd
        case possibleCmd of
            Cmd clientCmd -> do
                exchangeResp <- (sendNodeExchange . ccToNodeExchange) clientCmd
                sendResponse $ showExchangeResponse exchangeResp
            ErrorCmd err -> sendResponse err
            UnknownCmd   -> sendResponse "Unknown command"
        nextStep (BS.unpack input) loop

showExchangeResponse :: ClientExchangeResponse -> String
showExchangeResponse =
    \case
        StringResp message              -> message
        SubmitResp (Just transactionId) -> show transactionId
        SubmitResp Nothing              -> "Transaction has not been accepted"
        BalanceResp balance             -> show balance
        QueryResp wasAdded              -> show wasAdded
        StatusInfo nodeInfo             -> prettyPrintStatusInfo nodeInfo

prettyPrintStatusInfo :: NodeInfo -> String
prettyPrintStatusInfo NodeInfo {..} =
         "NodeId         : " <> show nId
    <> "\nTxPoolCount    : " <> show txPoolCount
    <> "\nNeighbour nodes: " <> show neighbourNodes
    <> "\nBlock count    : " <> show blockCount
    <> "\nBlocks Info    : " <> foldl (<>) "\n" blocksInfo
    <> "Ledger:\n" <> ledger

response :: String -> S.ByteString
response str = BL.toStrict (toByteString (str <> "\n"))

connectClient :: NodeConversation -> ClientConfig -> SecretKey -> IO ()
connectClient nc (ClientConfig {..}) sk = do
    listenUnixSocket "sockets" clientId (connect clientId sk nc)
