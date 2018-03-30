module Node.Data where

import           Block
import           Control.Concurrent
import           Exchange
import           Ledger
import           NodeCommandLine
import           Serokell.Communication.IPC
import           Transaction

data NodeState = NodeState
    { nodeConfig       :: NodeConfig
    , neighbours       :: [NodeId]
    , blockchain       :: MVar BlockChain
    , transactionPool  :: MVar [Transaction]
    , nodeLedger       :: MVar Ledger
    , broadcastChannel :: Chan Broadcast
    }

fetchNodeId :: NodeState -> NodeId
fetchNodeId = nodeId . nodeConfig

data AddBlockRes
 = BlockAdded [LedgerError] Block Ledger BlockChain
 | BlockNotAdded String
