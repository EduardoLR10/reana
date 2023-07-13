{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module RDG
  (
    RDG,
    RDGNode(..),
    PresenceCondition,
    rdgFromEdges,
    rdgFromVertices,
    emptyRDG,
    getRDGRoot,
    foldG,
  ) where

import Algebra.Graph 
import Algebra.Graph.ToGraph (isAcyclic)
import Data.Data

deriving instance Data a => Data (Graph a)

type PresenceCondition = Bool
type RDG a = Graph (RDGNode a)

data RDGNode a =
  RDGNode
  {
    value :: a,
    presenceCondition :: PresenceCondition
  }
  deriving (Typeable, Data, Eq, Ord)

emptyRDG :: RDG a
emptyRDG = empty

validateRDG :: Ord a => Graph (RDGNode a) -> Graph (RDGNode a)
validateRDG rdg =
  if isAcyclic rdg then
    rdg
  else error "Could not create RDG due to cycle being found"

rdgFromEdges :: Ord a => [(RDGNode a, RDGNode a)] -> Graph (RDGNode a)
rdgFromEdges = validateRDG . edges

rdgFromVertices :: Ord a => [RDGNode a] -> Graph (RDGNode a)
rdgFromVertices = validateRDG . vertices

getRDGRoot :: Graph (RDGNode a) -> RDGNode a
getRDGRoot = undefined

foldG :: b -> (a -> b) -> (b -> b -> b) -> Graph a -> b
foldG start leafs folding = foldg start leafs folding folding
