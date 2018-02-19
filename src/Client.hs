{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Client
    ( connectClient
    , NodeConversation(..)
    ) where

import Address
import ClientCommandLine
import Control.Concurrent
import Crypto.Sign.Ed25519
import Data.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BSC
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
    | EmptyCmd
    | UnknownCmd [BSC.ByteString]
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

toPossibleCmd :: S.ByteString -> PossibleCmd
toPossibleCmd bs =
    case BSC.words bs of
        ["status"]           ->  Cmd StatusCmd
        ["BALANCE", address] -> (Cmd . BalanceCmd . Address) address
        ["QUERY", txId]      -> (Cmd . QueryCmd . TransactionId) txId
        ["SUBMIT", address, amount] ->
            case DBC.fromByteString amount of
                Nothing -> ErrorCmd ("Amount must be a number, got: " <> (BSC.unpack amount))
                Just n  -> Cmd $ TransferCmd (Address address) n
        []   -> EmptyCmd
        unknown -> UnknownCmd unknown

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
                sendResponse $ showExchangeResponse (deriveAddress $ toPublicKey sk) exchangeResp
            ErrorCmd err -> sendResponse err
            UnknownCmd unknown -> sendResponse $ "Unknown command: " <> show unknown
            EmptyCmd -> pure ()
        nextStep (BSC.unpack input) loop

showExchangeResponse :: Address -> ClientExchangeResponse -> String
showExchangeResponse address =
    \case
        StringResp message              -> message
        SubmitResp (Just transactionId) -> show transactionId
        SubmitResp Nothing              -> "Transaction has not been accepted"
        BalanceResp balance             -> show balance
        QueryResp wasAdded              -> show wasAdded
        StatusInfo nodeInfo             -> prettyPrintStatusInfo address nodeInfo

prettyPrintStatusInfo :: Address ->NodeInfo -> String
prettyPrintStatusInfo address NodeInfo {..} =
         "NodeId         : " <> show nId
    <> "\nTxPoolCount    : " <> show txPoolCount
    <> "\nNeighbour nodes: " <> show neighbourNodes
    <> "\nBlock count    : " <> show blockCount
    <> "\nBlocks Info    : " <> foldl (<>) "\n" blocksInfo
    <> "Address:\n"  <> show address
    <> "\nLedger:\n" <> ledger

response :: String -> S.ByteString
response str = BL.toStrict (toByteString (str <> "\n"))

connectClient :: NodeConversation -> ClientConfig -> SecretKey -> IO ()
connectClient nc (ClientConfig {..}) sk = do
    listenUnixSocket "sockets" clientId (connect clientId sk nc)
