module Test.Utils where

import Address
import Block
import Control.Newtype
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Ledger
import Time
import Transaction
import Utils

keysFactory :: String -> PublicKey
keysFactory seed =
    case createKeypairFromSeed_ (C.pack seed) of
        Nothing -> error "seed has incorrect length"
        Just (pk, _) -> pk

testPk :: PublicKey
testPk = keysFactory "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

testPk2 :: PublicKey
testPk2 = keysFactory "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"

emptyLedger :: Ledger
emptyLedger = Ledger Map.empty

addLedger :: (PublicKey, Int) -> Ledger -> Ledger
addLedger (pk, amountToAdd) =
    over
        Ledger
        (Map.insert (deriveAddress pk) amountToAdd)

ledgerOf :: [(PublicKey, Int)] -> Ledger
ledgerOf = foldr addLedger emptyLedger

dummySignature :: Signature
dummySignature = Signature (C.pack "dummy-ignature")

dummyTxId :: TransactionId
dummyTxId = TransactionId (C.pack "dummy-txId")

dummyTimestamp :: Timestamp
dummyTimestamp = 0

mkTransaction :: PublicKey -> PublicKey -> Balance -> Transaction
mkTransaction fromPk itTo bal =
    Transaction dummyTxId testTransfer dummySignature dummyTimestamp
  where
    testTransfer = Transfer fromPk (deriveAddress itTo) bal

emptyBlock :: Block
emptyBlock = mkBlock []

mkBlock :: [Transaction] -> Block
mkBlock txs = Block {index = Index 1, transactions = txs, Block.timestamp = 0}
