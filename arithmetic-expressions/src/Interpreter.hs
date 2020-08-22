module Interpreter
  ( eval
  ) where

import Parser
import Control.Monad

eval :: Expr -> Maybe Integer
eval (Num x) = Just x
eval (Div e1 e2) = join $ safeDiv <$> eval e1 <*> eval e2
eval (Neg e1) = negate <$> eval e1
eval expr = getBinOp expr <$> eval (e1 expr) <*> eval (e2 expr)

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

getBinOp :: Expr -> Integer -> Integer -> Integer
getBinOp (Add _ _) = (+)
getBinOp (Sub _ _) = (-)
getBinOp (Mult _ _) = (*)
getBinOp _ = undefined
