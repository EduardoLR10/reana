{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ADD
  (
    ADD(..),
    evenExpr,
    oddExpr,
    one,
    eval,
    partialEval,
    ReliabilityExpr,
  ) where

import Data.Data
import Data.Map as Map
import Text.Printf

data ADD a = ADD a
  deriving (Data, Eq, Show, Ord, Functor)

instance Applicative ADD where
  pure = ADD
  (ADD f) <*> (ADD a) = ADD (f a) 

instance Monad ADD where
  return = pure
  (ADD a) >>= f = f a

data Expr
  = Lit Double
  | Label String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Data, Show, Eq)

type Context = Map.Map String Double
type ReliabilityExpr = (Context, Expr)

emptyContext :: Context
emptyContext = Map.empty

mergeContexts :: Context -> Context -> Context
mergeContexts = Map.union

evenExpr :: ReliabilityExpr
evenExpr = (ctx, (Add (Mul (Label "y") (Lit 5.0)) (Label "y")))
  where ctx = (insert "y" 2.0 emptyContext)

oddExpr :: ReliabilityExpr
oddExpr = (ctx, (Add (Add (Label "z") (Lit 3.0)) (Label "z")))
  where ctx = (insert "z" 5.0 emptyContext)

one :: ReliabilityExpr
one = (emptyContext, Lit 1.0)

binaryOpExpr :: (Double -> Double -> Double) -> Context -> Expr -> Expr -> (Context, Expr)
binaryOpExpr op ctx expr1 expr2 = case eval ctx expr1 of
                                    (ctx1, (Lit v1)) -> case eval ctx1 expr2 of
                                                          (ctx2, (Lit v2)) -> (ctx2, Lit (op v1 v2))
                                                          _ -> error (printf "Could not reduce operand to number form in %s" (show expr2))
                                    _ -> error (printf "Could not reduce operand to number form in %s" (show expr1))

eval :: Context -> Expr -> (Context, Expr)
eval ctx (Lit v)           = (ctx, Lit v)
eval ctx (Label s)         = maybe (error (printf "Label %s not found in map" s)) ((ctx,) . Lit . id) (Map.lookup s ctx)
eval ctx (Add expr1 expr2) = binaryOpExpr (+) ctx expr1 expr2
eval ctx (Sub expr1 expr2) = binaryOpExpr (-) ctx expr1 expr2
eval ctx (Mul expr1 expr2) = binaryOpExpr (*) ctx expr1 expr2
eval ctx (Div expr1 expr2) = binaryOpExpr (/) ctx expr1 expr2

partialEval' :: Context -> Expr -> Expr -> (Context, Expr)
partialEval' ctx _ expr = eval ctx expr

partialEval :: ADD ReliabilityExpr -> ADD ReliabilityExpr -> ADD ReliabilityExpr
partialEval (ADD (ctx1, expr1)) (ADD (ctx2, expr2)) = pure $ partialEval' (mergeContexts ctx1 ctx2) expr1 expr2 
