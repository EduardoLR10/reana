{-# LANGUAGE RankNTypes #-}
module Reana
  (featureFamilyAnalysis,
   UMLDiagram(..),
   parseDiagram,
   familyAnalysis,
   featureAnalysis
  ) where

import ADD
import UML
import RDG
import Util
import Data.Generics.Schemes
import Data.Typeable

data UMLDiagram = UMLDiagramStub
type FDTMC = Double

--------------------------------------- FEATURE ANALYSIS

parseDiagram :: UMLDiagram -> RDG SequenceFragment
parseDiagram _ = rdgFromVertices $ fmap nodeFromValue [s1, s2, s3]
  where s1 = emptySequenceFragment
        s2 = emptySequenceFragment
        s3 = emptySequenceFragment

intermediateTransformation :: SequenceFragment -> FDTMC
intermediateTransformation = const randomNumber

modelCheck :: FDTMC -> ReliabilityExpr
modelCheck fdtmc = if even (round fdtmc :: Integer) then evenExpr else oddExpr

featureAnalysis :: RDG SequenceFragment -> RDG ReliabilityExpr
featureAnalysis = fmap (fmap (modelCheck . intermediateTransformation))

--------------------------------------- FAMILY ANALYSIS

lift :: RDGNode ReliabilityExpr -> RDGNode (ADD ReliabilityExpr)
lift = fmap pure

familyAnalysis :: RDG ReliabilityExpr -> RDG (ADD ReliabilityExpr)
familyAnalysis = fmap lift

--------------------------------------- FEATURE-FAMILY ANALYSIS

partialEvalExpr :: ADD ReliabilityExpr -> ADD ReliabilityExpr -> ADD ReliabilityExpr
partialEvalExpr (ADD (ctx1, add1)) (ADD (ctx2, add2)) = pure $ partialEval (mergeContexts ctx1 ctx2) add1 add2
  
getExpr :: Typeable a => a -> (ADD ReliabilityExpr, Bool)
getExpr v = case cast v :: Maybe (RDGNode (ADD ReliabilityExpr)) of
              Nothing -> error "This should never happen"
              Just (RDGNode add pc) -> (add, not pc)

featureFamilyAnalysis :: UMLDiagram -> ADD ReliabilityExpr
featureFamilyAnalysis = (everythingBut partialEvalExpr getExpr) . familyAnalysis . featureAnalysis . parseDiagram

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
