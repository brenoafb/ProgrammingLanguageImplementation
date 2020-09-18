module Compiler where

import Parser
import Machine

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

-- a VarTable contains an assignment of each variable used in the program
-- to a register index in the machine.
-- Note that this language does not have lexical scoping, thus we treat
-- a variable 'x' appearing in any scope as the same variable.
type VarTable = Map.Map String Int
type VarTables = Map.Map String VarTable
type CompileT a = ExceptT Error (Reader Program) a
type Error = String

buildVarTables :: Program -> VarTables
buildVarTables fs = Map.fromList (zip names (map buildVarTable fs))
  where names = map (\(Function name _ _) -> name) fs

buildVarTable :: Function -> VarTable
buildVarTable f = Map.fromList $ zip (getVariables f) [0..]

getVariables :: Function -> [String]
getVariables (Function name args body) =
  unique (args ++ getStmtVariables body)

-- new (valid) variables can only show up in assignments
getStmtVariables :: Stmt -> [String]
getStmtVariables (Assignment s _) = [s]
getStmtVariables (If _ s) = getStmtVariables s
getStmtVariables (IfElse _ s1 s2) =
  unique $ getStmtVariables s1 ++ getStmtVariables s2
getStmtVariables (Block stmts) =
  unique $ concatMap getStmtVariables stmts
getStmtVariables (While _ s) = getStmtVariables s
getStmtVariables _ = []

-- getVariables s = case s of
--                    Assignment s _ -> [s]
--                    If _ s -> getVariables s
--                    IfElse _ s1 s2 -> unique $ getVariables s1 ++ getVariables s2
--                    Block stmts -> unique . concat $ map getVariables stmts
--                    While _ s -> getVariables s

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
