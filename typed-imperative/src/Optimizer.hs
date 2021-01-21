{-# LANGUAGE RankNTypes #-}
module Optimizer (optimize) where

import Syntax
import Data.Generics

optimize :: Program -> Program
optimize p = p2
  where p1 = foldr apply p exprOptimizations
        p2 = foldr apply p1 stmtOptimizations
        apply x acc = everywhere (mkT x) acc

-- optimizations :: [forall a. a -> a]
exprOptimizations =
  [ optimizeConstants
  , optimizeComparisons
  ]

stmtOptimizations =
  [ optimizeConditionals
  ]

optimizeConstants :: Expr -> Expr
optimizeConstants (BinOp Add  (Num x) (Num y)) = Num $ x + y
optimizeConstants (BinOp Sub  (Num x) (Num y)) = Num $ x - y
optimizeConstants (BinOp Mult (Num x) (Num y)) = Num $ x * y
optimizeConstants (BinOp And  EFalse  _      ) = EFalse
optimizeConstants (BinOp And  _       EFalse ) = EFalse
optimizeConstants (BinOp Or   ETrue   _      ) = ETrue
optimizeConstants (BinOp Or   _       ETrue  ) = ETrue
optimizeConstants x = x

optimizeComparisons :: Expr -> Expr
optimizeComparisons (RelOp Eq  (Num x) (Num y))
  | x == y = ETrue
  | x /= y = EFalse
optimizeComparisons (RelOp NEq (Num x) (Num y))
  | x == y = EFalse
  | x /= y = ETrue
optimizeComparisons (RelOp Eq  (Var x) (Var y))
  | x == y = ETrue
  | x /= y = EFalse
optimizeComparisons (RelOp NEq (Var x) (Var y))
  | x == y = EFalse
  | x /= y = ETrue

optimizeConditionals :: Stmt -> Stmt
optimizeConditionals (If ETrue body)            = body
optimizeConditionals (If EFalse body)           = Block []
optimizeConditionals (IfElse ETrue conseq alt)  = conseq
optimizeConditionals (IfElse EFalse conseq alt) = alt
optimizeCondionals x = x
