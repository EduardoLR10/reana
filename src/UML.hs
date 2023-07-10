module UML
  (
    ActivityDiagram(..)
  ) where

data NodeType
  = Decision
  | Statement
  | Termination
  deriving (Show, Eq, Ord)

data ActivityDiagram
  =
    ActivityDiagram { type'       :: NodeType,
                      name        :: String,
                      description :: String
            }
    deriving (Show, Eq, Ord)
