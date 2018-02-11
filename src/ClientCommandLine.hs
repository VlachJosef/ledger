module ClientCommandLine
    ( ClientConfig(..)
    , parseArguments
    ) where

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Options.Applicative
import OrphanedShow
import Serokell.Communication.IPC

data ClientConfig = ClientConfig
    { clientId   :: NodeId
    , nodeId     :: NodeId
    , socketDir  :: Maybe FilePath
    , keyPairDir :: Maybe FilePath
    } deriving (Show)

defaultClientArguments :: ClientConfig
defaultClientArguments = ClientConfig (NodeId 50) (NodeId 0) Nothing Nothing

clientArgumentsParser :: Parser ClientConfig
clientArgumentsParser =
    ClientConfig <$>
    (fromMaybe (clientId defaultClientArguments) <$> toNodeId clientIdParser) <*>
    (fromMaybe (nodeId defaultClientArguments) <$> toNodeId nodeIdParser) <*>
    socketsParser <*>
    keysParser

toNodeId :: Parser (Maybe Int) -> Parser (Maybe NodeId)
toNodeId p = (NodeId <$>) <$> p

clientIdParser :: Parser (Maybe Int)
clientIdParser =
    optional $
    option auto $
    long "clientId" <> short 'c' <> metavar "CLIENT_ID" <>
    help "Socket client id."

nodeIdParser :: Parser (Maybe Int)
nodeIdParser =
    optional $
    option auto $
    long "nodeId" <> short 'n' <> metavar "NODE_ID" <> help "Socket node id."

socketsParser :: Parser (Maybe FilePath)
socketsParser =
    optional $
    strOption $
    long "sockets" <> short 's' <> metavar "SOCKETS_DIR" <>
    help "Path to the directory with Unix sockets."

keysParser :: Parser (Maybe FilePath)
keysParser =
    optional $
    strOption $
    long "keys" <> short 'k' <> metavar "KEYS_DIR" <>
    help
        "Path to the directory with key pair. If missing current directory is assumed."

parseArguments :: ParserInfo ClientConfig
parseArguments =
    info
        (clientArgumentsParser <**> helper)
        (fullDesc <>
         progDesc "Once connected to nodeId you can tranfer your money" <>
         header "Client for blockchain money transfer")
