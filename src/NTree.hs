module NTree
  (
    NTree(..)
  ) where

data NTree a
  =
    Node { nodeValue :: a,
           children :: [NTree a]
         }
  | Leaf { leafValue :: a}
  deriving (Eq, Ord, Show)

instance Functor NTree where
  fmap f uml =
    case uml of
      (Node value cs) ->
        Node (f value) (map (fmap f) cs)
      (Leaf value) -> Leaf (f value)

