module NodeCommandLine
    ( NodeConfig(..)
    , parseArguments
    ) where

import Data.Semigroup ((<>))
import Options.Applicative
import OrphanedShow
import Serokell.Communication.IPC

data NodeConfig = NodeConfig
    { nodeId :: NodeId
    , nodeCount :: Int
    , socketDir :: String
    , disconnectTimeout :: Int
    , stabilityTimeout :: Int
    , resyncTimeout :: Int
    } deriving (Show)

nodeConfigParser :: Parser NodeConfig
nodeConfigParser =
    NodeConfig <$>
    (NodeId <$> argument auto (metavar "ID" <> help "Target for the greeting0")) <*>
    argument
        auto
        ((metavar "NODE_COUNT") <> (help "number of nodes in the network")) <*>
    strArgument
        (metavar "SOCKET_DIR" <> help "path to the directory with Unix sockets") <*>
    argument
        auto
        (metavar "DISCONNECT_TIMEOUT" <>
         help
             "Adversary can’t perform the disconnect operation more often than once in disconnectTimeout ms") <*>
    argument
        auto
        (metavar "STABILITY_TIMEOUT" <> help "Transaction stability timeout") <*>
    argument
        auto
        (metavar "RESYNC_TIMEOUT" <> help "Timeout for ledger synchronization")

parseArguments :: ParserInfo NodeConfig
parseArguments =
    (info
         (nodeConfigParser <**> helper)
         (fullDesc <> progDesc "Print a greeting for TARGET" <>
          header "hello - a test for optparse-applicative"))
