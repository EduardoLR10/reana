{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module ADD
  (
    ADD(..),
    partialEval,
    mergeContexts,
    evenExpr,
    oddExpr,
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

binaryOP :: (a -> b -> c) -> ADD a -> ADD b -> ADD c
binaryOP func da db = fmap func da <*> db

instance (Num a) => Num (ADD a) where
  x + y = binaryOP (+) x y
  x - y = binaryOP (-) x y
  x * y = binaryOP (*) x y
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger i = return $ fromInteger i

instance (Fractional a) => Fractional (ADD a) where
  x / y = binaryOP (/) x y
  recip = fmap recip
  fromRational t = return $ fromRational t

instance (Floating a) => Floating (ADD a) where
  pi = return pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  x ** y = binaryOP (**) x y
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

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
evenExpr = (ctx, (Add (Mul (Label "y") (Lit 5.0)) (Label "z")))
  where ctx = (insert "y" 2.0 emptyContext)

oddExpr :: ReliabilityExpr
oddExpr = (ctx, (Add (Add (Label "x") (Lit 3.0)) (Label "y")))
  where ctx = (insert "y" 5.0 emptyContext)

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

partialEval :: Context -> Expr -> Expr -> (Context, Expr)
partialEval ctx _ expr = eval ctx expr
