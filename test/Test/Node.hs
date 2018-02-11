module Test.Node where

import Block
import Control.Concurrent
import Node
import NodeCommandLine
import Serokell.Communication.IPC
import Test.Hspec
import Test.Utils

testNodeConfig :: NodeConfig
testNodeConfig = NodeConfig (NodeId 1) 5 "./sockets" 1 1 1

main :: IO ()
main =
    hspec $ do
        describe "Node" $ do
            describe "addBlock" $ do
                it "should return balance from Ledger" $
                    let tx = mkTransaction testPk testPk2 100
                        block = (mkBlock [tx]) {index = 2}
                    in do nodeState <- initialNodeState testNodeConfig
                          mBlock <- addBlock block nodeState
                          mBlock `shouldBe` Just block
                          ledger <- readMVar $ nodeLedger nodeState
                          ledger `shouldBe` ledgerOf [(testPk, 99900), (testPk2, 100)]
                it "should rewind last block if new block received is of older date" $
                    let tx = mkTransaction testPk testPk2 30
                        tx2 = mkTransaction testPk testPk2 50
                        firstBlock = (mkBlock [tx]) {index = 2, timestamp = 2}
                        secondBlock = (mkBlock [tx2]) {index = 2, timestamp = 1}
                    in do nodeState <- initialNodeState testNodeConfig
                          mBlock <- addBlock firstBlock nodeState
                          mBlock `shouldBe` Just firstBlock
                          ledger <- readMVar $ nodeLedger nodeState
                          ledger `shouldBe` ledgerOf [(testPk, 99970), (testPk2, 30)]
                          mBlock2 <- addBlock secondBlock nodeState
                          mBlock2 `shouldBe` Just secondBlock
                          ledger2 <- readMVar $ nodeLedger nodeState
                          ledger2 `shouldBe` ledgerOf [(testPk, 99950), (testPk2, 50)]
