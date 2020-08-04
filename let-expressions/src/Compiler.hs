module Compiler where

import Parser
import Machine

type VarTable = [(String, Int)]

compile :: VarTable -> Expr -> [OP]
compile t e = case e of
              Num x  -> [PUSH x]
              Neg e' -> compile t e' ++ [NEG]
              Var id  -> [LOAD i]
                where Just i = lookup id t
--              Let id e1 e2 -> let c1 = compile t e1
--                                  c2 = compile t e2
--                                  Just i = lookup id t
--                              in c1 ++ [STORE i] ++ c2
              Let p body -> let c = compile t body
                                cs = map (compileAssignment t) p
                             in concat cs ++ c
              _ -> let c1 = compile t $ e1 e
                       c2 = compile t $ e2 e
                       op = getOp e
                    in c1 ++ c2 ++ [op]

compileAssignment :: VarTable -> (String, Expr) -> [OP]
compileAssignment t (v, e) = let c = compile t e
                                 Just i = lookup v t
                              in c ++ [STORE i]

getVariables :: Expr -> [String]
getVariables e = case e of
                   Num _ -> []
                   Var x -> [x]
                   Let p body -> unique $ map fst p ++ getVariables body
                   Neg e1 -> getVariables e
                   _ -> unique $ getVariables (e1 e) ++ getVariables (e2 e)

assignRegisters :: [String] -> VarTable
assignRegisters vars = zip vars [0..]

getOp :: Expr -> OP
getOp (Mult _ _) = MUL
getOp (Div _ _)  = DIV
getOp (Add _ _)  = ADD
getOp (Sub _ _)  = SUB
getOp (Neg _)    = NEG
getOp _ = undefined

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if x `elem` xs
                   then unique xs
                   else x : unique xs
