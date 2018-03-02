module Node.Internal where

import Address
import Block
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NEL
import Ledger
import Transaction
import Node.Data
import Data.Semigroup

data BlockToLedger
    = BlockToLedger [LedgerError]
                    Ledger
                    Block
    | NoValidTransaction -- No valid transaction in a block to add to the ledger
    deriving (Eq, Show)

balance :: Ledger -> Address -> Either LedgerError Balance
balance (Ledger ledger) address =
    maybe (Left $ AddressNotFound address) Right (Map.lookup address ledger)

addBlockToLedger :: Block -> Ledger -> BlockToLedger
addBlockToLedger block ledger =
    let txs = transactions block
        (errors, ledgerUpd, validTransactions) = applyTransactions ledger txs
        validBlock = block {transactions = validTransactions}
    in if null validTransactions
           then NoValidTransaction
           else BlockToLedger errors ledgerUpd validBlock

applyTransaction
    :: Transaction
    -> ([LedgerError], Ledger, [Transaction])
    -> ([LedgerError], Ledger, [Transaction])
applyTransaction tx (errors, l@(Ledger ledger), validTransactions) =
    case updatedLedger of
        Left err -> (err : errors, l, validTransactions)
        Right led -> (errors, led, tx : validTransactions)
  where
    tran        = transfer tx
    fromAccount = (deriveAddress . from) tran
    toAccount   = to tran
    amountToPay = amount tran
    updatedLedger = do
        f <- balance l fromAccount
        if f >= amountToPay
            then let ledgerUpd  = Map.insertWith (flip (-)) fromAccount amountToPay ledger
                     ledgerUpd2 = Map.insertWith (+) toAccount amountToPay ledgerUpd
                 in Right $ Ledger ledgerUpd2
            else Left $ InsufficientBalance fromAccount toAccount amountToPay

applyTransactions :: Ledger
                  -> [Transaction]
                  -> ([LedgerError], Ledger, [Transaction])
applyTransactions ledger = foldr applyTransaction ([], ledger, [])

rewindBlock :: Ledger -> Block -> Ledger
rewindBlock ledger block = let
  txs = transactions block
  in foldr rewindTransaction ledger txs

rewindTransaction :: Transaction -> Ledger -> Ledger
rewindTransaction tx (Ledger ledger) = let
  fromAccount = (deriveAddress . from . transfer) tx
  toAccount   = (to . transfer) tx
  amountToPay = (amount . transfer) tx
  ledgerUpd   = Map.insertWith (+) fromAccount amountToPay ledger
  ledgerUpd2  = Map.insertWith (flip (-)) toAccount amountToPay ledgerUpd
  in Ledger ledgerUpd2

addReplaceBlock :: Block -> NodeState -> BlockChain -> Ledger -> AddBlockRes
addReplaceBlock block nodeState blocks ledger =
  let
     recievedBlockId   = index block
     currentBlock      = NEL.head blocks
     currentBlockIndex = index currentBlock
     expectedBlockId   = nextIndex currentBlockIndex
  in if expectedBlockId == recievedBlockId
     then
       case addBlockToLedger block ledger of
        BlockToLedger errors ledgerUpd addedBlock ->
          BlockAdded errors addedBlock ledgerUpd blocks
        NoValidTransaction ->
          BlockNotAdded $ "Block " <> show recievedBlockId <> " contains no valid transaction."
     else if currentBlockIndex == recievedBlockId && currentBlock /= block
          then let
            currentBlockTimeStamp = Block.timestamp currentBlock
            recievedBlockTimeStamp = Block.timestamp block
          in if recievedBlockTimeStamp < currentBlockTimeStamp then
               case snd $ NEL.uncons blocks of
                Just blocksTail -> addReplaceBlock block nodeState blocksTail (rewindBlock ledger currentBlock)
                Nothing -> BlockNotAdded $ "Error. Cannot replace genesis block."
             else BlockNotAdded $ "Received block index " <> show recievedBlockId
              <> " with timestamp " <> show recievedBlockTimeStamp
              <> " newer than current block timestamp " <> show currentBlockTimeStamp
              <> ". Keeping older one."
     else BlockNotAdded $ "Expecting block index " <> show expectedBlockId
          <> ". Received block index " <> show recievedBlockId
