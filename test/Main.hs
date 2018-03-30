module Main where

import qualified Test.Node          as Node
import qualified Test.Node.Internal as Node.Internal

main :: IO ()
main = do
    Node.Internal.main
    Node.main
