module Main where

import CommandLine
import Lib
import Options.Applicative

main :: IO ()
main = www =<< execParser aaaa
-- greet :: CLIArguments -> IO ()
-- greet cli = www
