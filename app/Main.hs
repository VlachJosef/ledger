module Main where

import Node
import NodeCommandLine
import Options.Applicative

main :: IO ()
main = (www . nodeId) =<< execParser aaaa
