{-# LANGUAGE RankNTypes #-}
module Reana (featureFamilyAnalysis) where

import UML
import RDG
import Data.Generics.Schemes
import Data.Typeable
import Data.Data

data UMLDiagram = UMLDiagramStub

data FDTMC = FDTMCStub
data Feature = FeatureStub
data SystemConfiguration = SystemConfigStub

data ADD a = ADDStub a
  deriving Data

data Expr
  = Lit Double
  | Label String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Data, Show, Eq)

type ReliabilityExpr = Expr
type Reliability = Double

--------------------------------------- FEATURE ANALYSIS

readDiagram :: UMLDiagram -> RDG SequenceFragment
readDiagram = undefined

intermediateTransformation :: RDGNode SequenceFragment -> RDGNode FDTMC
intermediateTransformation = undefined

checkModel :: RDGNode FDTMC -> RDGNode ReliabilityExpr
checkModel = undefined

featureAnalysis :: RDG SequenceFragment -> RDG ReliabilityExpr
featureAnalysis = fmap (checkModel . intermediateTransformation)

--------------------------------------- FAMILY ANALYSIS

lift :: RDGNode ReliabilityExpr -> RDGNode (ADD ReliabilityExpr)
lift = undefined

familyAnalysis :: RDG ReliabilityExpr -> RDG (ADD ReliabilityExpr)
familyAnalysis = fmap lift

--------------------------------------- SYB

combineADD :: ADD ReliabilityExpr -> ADD ReliabilityExpr -> ADD ReliabilityExpr
combineADD = undefined

mkPairADD :: Typeable a => a -> (ADD ReliabilityExpr, Bool)
mkPairADD v = case cast v :: Maybe (RDGNode (ADD ReliabilityExpr)) of
                Nothing -> error "This should never happen"
                Just (RDGNode add pc) -> (add, pc)

evalADD :: ADD ReliabilityExpr -> Reliability
evalADD = undefined

foldRDG :: RDG (ADD ReliabilityExpr) -> Reliability
foldRDG rdg = evalADD $ everythingBut combineADD mkPairADD rdg

--------------------------------------- ALTERNATIVE WAY

evalADD' :: RDGNode (ADD ReliabilityExpr) -> Reliability
evalADD' (RDGNode _ False) = 1.0
evalADD' (RDGNode add True) = evalADD add

foldRDG' :: RDG (ADD ReliabilityExpr) -> Reliability
foldRDG' rdg = foldG (evalADD . value $ getRDGRoot rdg) evalADD' (*) rdg

--------------------------------------- FEATURE-FAMILY ANALYSIS

updatePresenceConditions :: SystemConfiguration -> RDG (ADD ReliabilityExpr) -> RDG (ADD ReliabilityExpr)
updatePresenceConditions = undefined

featureFamilyAnalysis :: SystemConfiguration -> UMLDiagram -> Reliability
featureFamilyAnalysis system = foldRDG . (updatePresenceConditions system) . familyAnalysis . featureAnalysis . readDiagram
