module NcCommandLine
    (NcConfig(..)
    , parseArguments
    ) where

import Data.Semigroup ((<>))
import Options.Applicative
import Serokell.Communication.IPC

data NcConfig = NcConfig
    { clientId :: NodeId
    }

toNodeId :: Parser Int -> Parser NodeId
toNodeId = (<$>) NodeId

configParser :: Parser NcConfig
configParser = NcConfig <$> toNodeId nodeIdParser

nodeIdParser :: Parser Int
nodeIdParser = argument auto (metavar "CLIENT_ID" <> help "NodeId of the client to connect to.")

desc :: String
desc = "Connect to client, send command and print response."

parseArguments :: ParserInfo NcConfig
parseArguments =
    info
         (configParser <**> helper)
         (fullDesc <> progDesc desc <>
          header desc)
