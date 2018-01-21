module Main where

import CommandLine
import Lib
import Options.Applicative

main :: IO ()
main = www =<< execParser aaaa
