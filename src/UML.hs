{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module UML
  (
    SequenceFragment,
    emptySequenceFragment
  ) where

import Data.Graph.Types
import Data.Graph.DGraph
import Data.Data
import Data.Hashable
import GHC.Generics (Generic)

type SequenceFragment = DGraph SequenceNode SequenceEdge

emptySequenceFragment :: SequenceFragment
emptySequenceFragment = empty

instance (Hashable v, Ord v, Ord e) => Ord (DGraph v e) where
    d1 <= d2 = (size d1) <= (size d2)
    
-- deriving instance (Data a, Data b) => Data (DGraph a b)

data SequenceNode
  =
    SequenceNode
    { stage :: String,
      idN :: Int
    }
    deriving (Show, Data, Eq, Ord, Generic)

instance Hashable SequenceNode

data SequenceEdge
  =
    SequenceEdge
    { idE :: Int,
      name :: String,
      probability :: Double
    }
    deriving (Show, Data, Eq, Ord, Generic)

instance Hashable SequenceEdge
