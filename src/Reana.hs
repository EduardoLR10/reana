{-# LANGUAGE RankNTypes #-}
module Reana (reana) where

import ADD
import UML
import RDG
import Data.Generics.Schemes
import Data.Typeable
import System.Random
import System.IO.Unsafe

data UMLDiagram = UMLDiagramStub
type FDTMC = Double

--------------------------------------- FEATURE ANALYSIS

parseDiagram :: UMLDiagram -> RDG SequenceFragment
parseDiagram _ = rdgFromVertices $ fmap nodeFromValue [s1, s2, s3]
  where s1 = emptySequenceFragment
        s2 = emptySequenceFragment
        s3 = emptySequenceFragment

intermediateTransformation :: SequenceFragment -> FDTMC
intermediateTransformation = const (unsafePerformIO randomIO)

modelCheck :: FDTMC -> ReliabilityExpr
modelCheck fdtmc = if even (round fdtmc :: Integer) then evenExpr else oddExpr

featureAnalysis :: RDG SequenceFragment -> RDG ReliabilityExpr
featureAnalysis = fmap (modelCheck . intermediateTransformation)

--------------------------------------- FAMILY ANALYSIS

lift :: ReliabilityExpr -> ADD ReliabilityExpr
lift = pure

familyAnalysis :: RDG ReliabilityExpr -> RDG (ADD ReliabilityExpr)
familyAnalysis = fmap lift

--------------------------------------- FEATURE-FAMILY ANALYSIS

partialEvalExpr :: ADD ReliabilityExpr -> ADD ReliabilityExpr -> ADD ReliabilityExpr
partialEvalExpr (ADD (ctx, expr)) _ = pure $ eval ctx expr
  
getExpr :: Typeable a => a -> (ADD ReliabilityExpr, Bool)
getExpr v = case cast v :: Maybe (RDG (ADD ReliabilityExpr)) of
              Nothing -> error "This should never happen"
              Just Nil -> (pure one, False)
              Just (Cons (RDGNode add pc) _) -> (add, not pc)

featureFamilyAnalysis :: UMLDiagram -> ADD ReliabilityExpr
featureFamilyAnalysis = (everythingBut partialEvalExpr getExpr) . familyAnalysis . featureAnalysis . parseDiagram

reana :: ADD ReliabilityExpr
reana = featureFamilyAnalysis UMLDiagramStub

--------------------------------------- ALTERNATIVE WAY

-- evalADD' :: RDGNode (ADD ReliabilityExpr) -> Reliability
-- evalADD' (RDGNode _ False) = 1.0
-- evalADD' (RDGNode add True) = evalADD add

-- foldRDG' :: RDG (ADD ReliabilityExpr) -> Reliability
-- foldRDG' rdg = foldG (evalADD . value $ getRDGRoot rdg) evalADD' (*) rdg

--------------------------------------- ALTERNATIVE WAY

-- featureFamilyAnalysis' :: a -> (ADD ReliabilityExpr, Bool)
-- featureFamilyAnalysis' v = case cast v :: Maybe (RDGNode SequenceFragment) of
--                              Nothing -> error "This should never happen"
--                              Just rdgNode -> let (RDGNode add pc) = lift . modelCheck $ intermediateTransformation rdgNode
--                                              in (add, not pc)

-- reana :: UMLDiagram -> ADD ReliabilityExpr
-- reana = (everythingBut partialEvalExpr featureFamilyAnalysis') . parseDiagram
