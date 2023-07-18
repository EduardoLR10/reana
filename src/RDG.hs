{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module RDG
  (
    RDG(..),
    RDGNode(..),
    PresenceCondition,
    rdgFromVertices,
    nodeFromValue,
    emptyRDG,
    getRDGRoot
  ) where

import Data.Data


type PresenceCondition = Bool
data RDG a
  = Nil
  | Cons (RDGNode a) (RDG a)
  deriving (Typeable, Data, Eq, Ord, Show, Functor)
 
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
emptyRDG = Nil

isAcyclic :: RDG a -> Bool
isAcyclic = const True

validateRDG :: RDG a -> RDG a
validateRDG rdg =
  if isAcyclic rdg then
    rdg
  else error "Could not create RDG due to cycle being found"

rdgFromVertices :: Ord a => [RDGNode a] -> RDG a
rdgFromVertices [] = Nil
rdgFromVertices (x:xs) = validateRDG $ Cons x (rdgFromVertices xs)

nodeFromValue :: a -> RDGNode a
nodeFromValue = (\v -> (RDGNode v True))

getRDGRoot :: RDG a -> RDGNode a
getRDGRoot = undefined

-- foldG :: b -> (a -> b) -> (b -> b -> b) -> Graph a -> b
-- foldG start leafs folding = foldg start leafs folding folding
