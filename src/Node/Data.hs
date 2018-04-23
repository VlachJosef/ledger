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

toInterNodeId :: NodeId -> NodeId
toInterNodeId = NodeId . (+1000) . unNodeId

data AddBlockRes
 = BlockAdded [LedgerError] Block Ledger BlockChain
 | BlockNotAdded String
