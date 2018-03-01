module Node.Data where

import Serokell.Communication.IPC
import Transaction
import NodeCommandLine
import Control.Concurrent
import Block
import Ledger
import Exchange

data NodeState = NodeState
    { nodeConfig       :: NodeConfig
    , neighbours       :: [NodeId]
    , blockchain       :: MVar BlockChain
    , transactionPool  :: MVar [Transaction]
    , nodeLedger       :: MVar Ledger
    , broadcastChannel :: Chan Broadcast
    }

data AddBlockRes
 = BlockAdded [LedgerError] Block Ledger BlockChain
 | BlockNotAdded String
