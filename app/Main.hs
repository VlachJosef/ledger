module Main where

import           Node                (establishClusterConnection)
import           NodeCommandLine     (parseArguments)
import           Options.Applicative (execParser)

main :: IO ()
main = establishClusterConnection =<< execParser parseArguments
