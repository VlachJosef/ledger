module Test.Node.Internal where

import Address
import Ledger
import Node.Internal
import Test.Hspec
import Test.Utils

main :: IO ()
main =
    hspec $ do
        describe "Node.Internal" $ do
            describe "addBlockToLedger" $ do
                it
                    "should return NoValidTransaction if block contains no valid transaction" $
                    let block = emptyBlock
                        ledgerInitial = emptyLedger
                    in do addBlockToLedger block ledgerInitial `shouldBe`
                              NoValidTransaction
                it "should add all valid transactions from block to ledger" $
                    let tx = mkTransaction testPk testPk2 10
                        block = mkBlock [tx]
                        ledgerInitial = ledgerOf [(testPk, 100)]
                        ledgerExpected = ledgerOf [(testPk, 90), (testPk2, 10)]
                    in do addBlockToLedger block ledgerInitial `shouldBe`
                              BlockToLedger [] ledgerExpected block
                it
                    "should add all valid transactions from block to ledger and return block without invalid transactions" $
                    let tx = mkTransaction testPk testPk2 10
                        tx2 = mkTransaction testPk2 testPk 120
                        blockInitial = mkBlock [tx, tx2]
                        blockExpected = mkBlock [tx]
                        ledgerInitial = ledgerOf [(testPk, 100), (testPk2, 100)]
                        ledgerExpected = ledgerOf [(testPk, 90), (testPk2, 110)]
                        errorExpected =
                            InsufficientBalance
                                (deriveAddress testPk2)
                                (deriveAddress testPk)
                                120
                    in do addBlockToLedger blockInitial ledgerInitial `shouldBe`
                              BlockToLedger
                                  [errorExpected]
                                  ledgerExpected
                                  blockExpected
            describe "applyTransactions" $ do
                it "should apply multiple transactions" $
                    let tx = mkTransaction testPk testPk2 10
                        tx2 = mkTransaction testPk2 testPk 20
                        ledgerInitial = ledgerOf [(testPk, 100), (testPk2, 100)]
                        ledgerExpected = ledgerOf [(testPk, 110), (testPk2, 90)]
                    in do applyTransactions ledgerInitial [tx, tx2] `shouldBe`
                              ([], ledgerExpected, [tx, tx2])
            describe "applyTransaction" $ do
                it
                    "should return AddressNotFound if address from transaction is not in a ledger" $
                    let tx = mkTransaction testPk testPk 10
                        address = deriveAddress testPk
                    in do applyTransaction tx ([], emptyLedger, []) `shouldBe`
                              ([AddressNotFound address], emptyLedger, [])
                it "should add transaction into ledger" $
                    let tx = mkTransaction testPk testPk2 10
                        ledgerInitial = ledgerOf [(testPk, 100)]
                        ledgerExpected = ledgerOf [(testPk, 90), (testPk2, 10)]
                    in do applyTransaction tx ([], ledgerInitial, []) `shouldBe`
                              ([], ledgerExpected, [tx])
                it
                    "should add transaction into ledger (and allow transfer whole balance from an account)" $
                    let tx = mkTransaction testPk testPk2 100
                        ledgerInitial = ledgerOf [(testPk, 100)]
                        ledgerExpected = ledgerOf [(testPk, 0), (testPk2, 100)]
                    in do applyTransaction tx ([], ledgerInitial, []) `shouldBe`
                              ([], ledgerExpected, [tx])
                it "should return Insufficient balance error" $
                    let tx = mkTransaction testPk testPk2 101
                        ledgerInitial = ledgerOf [(testPk, 100)]
                        errorExpected =
                            InsufficientBalance
                                (deriveAddress testPk)
                                (deriveAddress testPk2)
                                101
                    in do applyTransaction tx ([], ledgerInitial, []) `shouldBe`
                              ([errorExpected], ledgerInitial, [])
            describe "balance" $ do
                it "should return balance from Ledger" $
                    let ledger = ledgerOf [(testPk, 123)]
                        address = deriveAddress testPk
                    in do balance ledger address `shouldBe` Right 123
                it
                    "should return AddressNotFound is address cannot be found in Ledger" $
                    let address = deriveAddress testPk
                    in do balance emptyLedger address `shouldBe`
                              Left (AddressNotFound address)

            describe "rewindBlock" $ do
              it "should rewind transaction" $
                let ledger = ledgerOf [(testPk, 123), (testPk2, 10)]
                    tx = mkTransaction testPk testPk2 10
                    block = mkBlock [tx]
                    ledgerExpected = ledgerOf [(testPk, 133), (testPk2, 0)]
                in do rewindBlock ledger block `shouldBe` ledgerExpected
              it "should rewind transaction 2" $
                let ledger = ledgerOf [(testPk, 123), (testPk2, 100)]
                    tx = mkTransaction testPk testPk2 50
                    tx2 = mkTransaction testPk testPk2 50
                    block = mkBlock [tx, tx2]
                    ledgerExpected = ledgerOf [(testPk, 223), (testPk2, 0)]
                in do rewindBlock ledger block `shouldBe` ledgerExpected
