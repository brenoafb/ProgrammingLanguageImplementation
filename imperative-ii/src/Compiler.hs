module Compiler where

import Parser
import Machine

import Control.Monad.Reader
import qualified Data.Map as Map

-- a VarTable contains an assignment of each variable used in the program
-- to a register index in the machine.
-- Note that this language does not have lexical scoping, thus we treat
-- a variable 'x' appearing in any scope as the same variable.
type VarTable = Map.Map String Int

compile' :: Stmt -> [OP]
compile' stmt = runReader (compile stmt) (buildVarTable stmt) ++ [HALT]

compile :: Stmt -> Reader VarTable [OP]
compile s = case s of
              Assignment var expr -> do
                c <- compileExpr expr
                t <- ask
                let Just i = Map.lookup var t -- warning: assumes vartable contains variable
                return $ c ++ [STORE i]
              If cond conseq -> do
                cTest <- compileExpr cond
                cBody <- compile conseq
                return $ cTest ++ [BZ (length cBody)] ++ cBody -- watch out for off-by-one error
              IfElse cond conseq alt -> do
                cTest <- compileExpr cond
                cBody <- compile conseq
                cAlt <- compile alt
                return $ cTest ++ [BZ (length cBody)] ++ cBody ++ cAlt-- watch out for off-by-one error
              While cond body -> do
                cTest <- compileExpr cond
                cBody <- compile body
                let d1 = 2 + length cBody
                    d2 = negate $ 1 + length cBody + length cTest  -- not sure if this is right
                return $ cTest ++ [BZ d1] ++ cBody ++ [GOTO d2]
              Block [] -> return []
              Block (stmt:stmts) -> (++) <$> compile stmt <*> compile (Block stmts)

compileExpr :: Expr -> Reader VarTable [OP]
compileExpr e = case e of
              Num x  -> return [PUSH x]
              Neg e' -> (++ [NEG]) <$> compileExpr e'
              Var id -> do
                t <- ask
                let Just i = Map.lookup id t -- warning: assumes vartable contains variable
                    op = getOp e
                return [LOAD i]
              _ -> do
                c1 <- compileExpr $ e1 e
                c2 <- compileExpr $ e2 e
                let op = getOp e
                return $ c1 ++ c2 ++ [op]

-- compileAssignment :: VarTable -> (String, Expr) -> [OP]
-- compileAssignment t (v, e) = let c = compile t e
--                                  Just i = lookup v t
--                               in c ++ [STORE i]

buildVarTable :: Stmt -> VarTable
buildVarTable s = let variables = getVariables s
                   in Map.fromList $ zip variables [0..]

getVariables :: Stmt -> [String]
getVariables s = case s of
                   Assignment s _ -> [s]
                   If _ s -> getVariables s
                   IfElse _ s1 s2 -> unique $ getVariables s1 ++ getVariables s2
                   Block stmts -> unique . concat $ map getVariables stmts
                   While _ s -> getVariables s

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
