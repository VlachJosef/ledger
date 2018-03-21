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
    | UnknownCmd [BSC.ByteString]
    | ErrorCmd String
    deriving Show

-- | CLI -> Client commands
data ClientCmd
    = StatusCmd
    | BalanceCmd Address
    | QueryCmd TransactionId
    | TransferCmd PublicKey
                  Int
    deriving Show

newtype NodeConversation = NodeConversation
    { unNodeConversation :: Conversation
    }

toPossibleCmd :: S.ByteString -> PossibleCmd
toPossibleCmd bs =
    case BSC.words bs of
        ["status"]                  -> Cmd StatusCmd
        ["BALANCE", address]        -> (Cmd . BalanceCmd . Address) address
        ["QUERY", txId]             -> (Cmd . QueryCmd . TransactionId) txId
        ["SUBMIT", address, amount] ->
          case DBC.fromByteString amount of
            Nothing -> ErrorCmd ("Amount must be a number, got: " <> BSC.unpack amount)
            Just n  -> Cmd $ TransferCmd (decodePublicKey address) n
        unknown -> UnknownCmd unknown

clientCmdToNodeExchange :: SecretKey -> ClientCmd -> Either ClientExchangeCLI ClientExchange
clientCmdToNodeExchange sk clientCmd =
    case clientCmd of
        StatusCmd             -> Left FetchStatus
        BalanceCmd address    -> Left $ AskBalanceByAddress address
        QueryCmd txId         -> Right $ Query txId
        TransferCmd pk amount -> Right $ MakeTransfer $ InitiateTransfer sk pk amount

sendEitherExchange :: NodeConversation -> Either ClientExchangeCLI ClientExchange -> IO (Either ClientExchangeCLIResponse ClientExchangeResponse)
sendEitherExchange nc (Left a)  = Left  <$> sendExchangeCLI nc a
sendEitherExchange nc (Right a) = Right <$> sendExchange nc a

sendExchange :: NodeConversation -> ClientExchange -> IO ClientExchangeResponse
sendExchange (NodeConversation Conversation {..}) exchange =
    case exchange of
       MakeTransfer _ -> sendAndProcessRespBy (runGetStrict encodeMakeTransferResp)
       Query _        -> sendAndProcessRespBy (runGetStrict encodeQueryResp)
       AskBalance _   -> sendAndProcessRespBy (runGetStrict encodeBalanceResp)
    where
      encodedExchange = (BL.toStrict . encode . ClientExchange) exchange
      sendAndProcessRespBy respDecoder = do resp <- send encodedExchange *> recvAll recv
                                            pure $ respDecoder resp
sendExchangeCLI :: NodeConversation -> ClientExchangeCLI -> IO ClientExchangeCLIResponse
sendExchangeCLI (NodeConversation Conversation {..}) exchange =
  let encodedExchangeCLI = (BL.toStrict . encode . ClientExchangeCLI) exchange
  in do resp <- send encodedExchangeCLI *> recvAll recv
        case decodeClientExchangeCLIResponse resp of
          Right (_, _, res) -> pure res
          Left _ -> error "Decoding of ClientExchangeCLIResponse failed"

connect :: NodeId -> SecretKey -> NodeConversation -> Conversation -> IO Bool
connect clientId sk nc Conversation {..} =
  True <$
    forkIO (logThread ("Client " <> showNodeId clientId <> ". Forking new thread!") <* loop)
  where
    sendNodeExchange = sendEitherExchange nc
    ccToNodeExchange = clientCmdToNodeExchange sk
    sendResponse = send . response
    loop :: IO ()
    loop = do
        input <- recv
        if null (BSC.unpack input)
          then logThread "Closed by peer!!!"
          else do
            let possibleCmd = toPossibleCmd input
            logThread $ "Command received: " <> show possibleCmd
            case possibleCmd of
                Cmd clientCmd -> do
                    exchangeResp <- (sendNodeExchange . ccToNodeExchange) clientCmd
                    sendResponse $ showExchangeResponse (deriveAddress $ toPublicKey sk) exchangeResp
                ErrorCmd err -> sendResponse err
                UnknownCmd unknown -> sendResponse $ "Unknown command: " <> show unknown
            loop

showExchangeResponse :: Address -> Either ClientExchangeCLIResponse ClientExchangeResponse -> String
showExchangeResponse address =
    \case
        Right (SubmitResp (Just transactionId)) -> show transactionId
        Right (SubmitResp Nothing)              -> "Transaction has not been accepted"
        Right (BalanceResp balance)             -> show balance
        Right (QueryResp wasAdded)              -> show wasAdded
        Left (StatusInfo nodeInfo)              -> prettyPrintStatusInfo address nodeInfo
        Left (BalanceRespCLI balance)           -> show balance

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
connectClient nc ClientConfig {..} sk =
    listenUnixSocket "sockets" clientId (connect clientId sk nc)
