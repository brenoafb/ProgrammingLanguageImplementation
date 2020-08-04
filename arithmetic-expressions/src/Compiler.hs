module Compiler where

import Machine
import Parser

compile :: Expr -> Program
compile expr = case expr of
                 Num x -> [PUSH x]
                 Neg e -> compile e ++ [NEG]
                 _ -> let c1 = compile $ e1 expr
                          c2 = compile $ e2 expr
                          op = getOp expr
                       in c1 ++ c2 ++ [op]

getOp :: Expr -> Op
getOp (Mult _ _) = MUL
getOp (Div _ _)  = DIV
getOp (Add _ _)  = ADD
getOp (Sub _ _)  = SUB
getOp (Neg _)    = NEG
getOp _ = undefined
