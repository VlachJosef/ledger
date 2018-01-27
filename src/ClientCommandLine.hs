module ClientCommandLine where

import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Options.Applicative
import Serokell.Communication.IPC

instance Show NodeId where
    show (NodeId id) = show id

data ClientConfig = ClientConfig
    { nodeId :: NodeId
    , socketDir :: Maybe FilePath
    , keyPairDir :: Maybe FilePath
    } deriving (Show)

defaultClientArguments :: ClientConfig
defaultClientArguments = ClientConfig (NodeId 1) Nothing Nothing

clientArgumentsParser :: Parser ClientConfig
clientArgumentsParser =
    ClientConfig <$>
    (fromMaybe (nodeId defaultClientArguments) <$> toNodeId nodeIdParser) <*>
    socketsParser <*>
    keysParser

toNodeId :: Parser (Maybe Int) -> Parser (Maybe NodeId)
toNodeId p = (NodeId <$>) <$> p

nodeIdParser :: Parser (Maybe Int)
nodeIdParser =
    optional $
    option auto $
    long "nodeId" <> short 'n' <> metavar "NODE_ID" <> help "Node Id to connect"

socketsParser :: Parser (Maybe FilePath)
socketsParser =
    optional $
    strOption $
    long "sockets" <> short 's' <> metavar "SOCKETS_DIR" <>
    help "Path to the directory with Unix sockets"

keysParser :: Parser (Maybe FilePath)
keysParser =
    optional $
    strOption $
    long "keys" <> short 'k' <> metavar "KEYS_DIR" <>
    help
        "Path to the directory with key pair. If missing current directory is assumed"

parseArguments :: ParserInfo ClientConfig
parseArguments =
    (info
         (clientArgumentsParser <**> helper)
         (fullDesc <>
          progDesc "Once connected to nodeId you can tranfer your money" <>
          header "Client for blockchain money transfer"))
