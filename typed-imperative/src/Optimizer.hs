module Optimizer (optimize) where

import Syntax
import Data.Generics

optimize :: Program -> Program
optimize p = foldr (\x acc -> everywhere (mkT x) acc) p optimizations

optimizations = [optimizeConstants]

optimizeConstants :: Expr -> Expr
optimizeConstants (BinOp Add (Num x) (Num y)) =
  Num $ x + y
optimizeConstants x = x
