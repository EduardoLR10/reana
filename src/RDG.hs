{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module RDG
  (
    RDG,
    RDGNode(..),
    PresenceCondition,
    rdgFromVertices,
    nodeFromValue,
    emptyRDG,
    getRDGRoot
  ) where

-- import Algebra.Graph 
-- import Algebra.Graph.ToGraph (isAcyclic)
import Data.Data

-- deriving instance Data a => Data (Graph a)

type PresenceCondition = Bool
--type RDG a = Graph (RDGNode a)
type RDG a = [RDGNode a]

data RDGNode a =
  RDGNode
  {
    value :: a,
    presenceCondition :: PresenceCondition
  }
  deriving (Typeable, Data, Eq, Ord, Show)

instance Functor RDGNode where
  fmap f (RDGNode v p) = (RDGNode (f v) p)

emptyRDG :: RDG a
emptyRDG = []

isAcyclic :: Ord a => RDG a -> Bool
isAcyclic = const True

validateRDG :: Ord a => RDG a -> RDG a
validateRDG rdg =
  if isAcyclic rdg then
    rdg
  else error "Could not create RDG due to cycle being found"

rdgFromVertices :: Ord a => [RDGNode a] -> RDG a
rdgFromVertices = validateRDG

nodeFromValue :: a -> RDGNode a
nodeFromValue = (\v -> (RDGNode v True))

getRDGRoot :: RDG a -> RDGNode a
getRDGRoot = undefined

-- foldG :: b -> (a -> b) -> (b -> b -> b) -> Graph a -> b
-- foldG start leafs folding = foldg start leafs folding folding
