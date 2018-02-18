module OrphanedShow where

import Serokell.Communication.IPC (NodeId(..))

instance Show NodeId where
    show (NodeId nId) = show nId
