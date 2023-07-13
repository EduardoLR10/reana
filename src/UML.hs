module UML
  (
    SequenceFragment
  ) where

import Data.Graph.DGraph

type SequenceFragment = DGraph SequenceNode SequenceEdge

data SequenceNode
  =
    SequenceNode
    { stage :: String,
      idN :: Int
    }
    deriving (Show, Eq)

data SequenceEdge
  =
    SequenceEdge
    { idE :: Int,
      name :: String,
      probability :: Double
    }
    deriving (Show, Eq)
