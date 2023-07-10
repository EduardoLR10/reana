module Reana
  (
    featureFamilyAnalysis
  ) where

import NTree
import UML
import RDG

type Model = NTree ActivityDiagram
data FDTMC = FDTMCStub
data Feature = FeatureStub
data SystemConfiguration = SystemConfigStub

data ADD a = ADDStub a
instance Semigroup (ADD a) where
  (<>) = undefined
instance Monoid (ADD a) where
  mappend = (<>)
  mempty = undefined

data Expr
  = Lit Double
  | Label String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

type ReliabilityExpr = Expr
type Reliability = Double

modelToRDG :: Model -> RDG FDTMC
modelToRDG = undefined

checkModel :: FDTMC -> ReliabilityExpr
checkModel = undefined

featureBasedAnalysis :: RDG FDTMC -> RDG ReliabilityExpr
featureBasedAnalysis = fmap checkModel

---------------------------------------

lift :: ReliabilityExpr -> ADD (Maybe Reliability)
lift = undefined

unvary :: SystemConfiguration -> ADD (Maybe Reliability) -> ADD Reliability
unvary = undefined

familyBasedAnalysis :: SystemConfiguration -> RDG ReliabilityExpr -> RDG (ADD Reliability)
familyBasedAnalysis sys = (fmap (unvary sys)) . (fmap lift)

---------------------------------------

computeReliability :: Feature -> (ADD Reliability) -> Reliability
computeReliability = undefined

featureFamilyAnalysis :: SystemConfiguration -> Feature -> Model -> Reliability
featureFamilyAnalysis sys feature = evaluate . (familyBasedAnalysis sys) . featureBasedAnalysis . modelToRDG
  where evaluate = foldRDG 0 (computeReliability feature) (+)
