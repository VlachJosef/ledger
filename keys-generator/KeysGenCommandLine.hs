module KeysGenCommandLine
    ( KeyGenConfig(..)
    , parseArguments
    ) where

import Data.Semigroup ((<>))
import Options.Applicative

newtype KeyGenConfig = KeyGenConfig
    { numberOfKeys :: Int
    } deriving (Show)

nodeConfigParser :: Parser KeyGenConfig
nodeConfigParser =
    KeyGenConfig <$> argument auto (metavar "NUMBER_OF_KEYS" <> help "Number of keys to generate.")

parseArguments :: ParserInfo KeyGenConfig
parseArguments =
    info
        (nodeConfigParser <**> helper)
        (fullDesc <> progDesc "Distribution file and private keys generator." <>
         header "Distribution file and private keys generator. It takes one parameter (number) and it will generate that amount of private keys.")
