module RDG
  (
    RDG,
    rdgFromEdges,
    rdgFromVertices,
    foldRDG,
    emptyRDG
  ) where

import Algebra.Graph (Graph, empty, edges, vertices, foldg)
import Algebra.Graph.ToGraph (isAcyclic)

type RDG a = Graph a

emptyRDG :: RDG a
emptyRDG = empty

validateRDG :: Ord a => RDG a -> RDG a
validateRDG rdg =
  if (isAcyclic rdg) then
    rdg
  else error "Could not create RDG due to cycle being found"

rdgFromEdges :: Ord a => [(a,a)] -> RDG a
rdgFromEdges = validateRDG . edges

rdgFromVertices :: Ord a => [a] -> RDG a
rdgFromVertices = validateRDG . vertices

foldRDG :: b -> (a -> b) -> (b -> b -> b) -> RDG a -> b
foldRDG start leafs folding = foldg start leafs folding folding
