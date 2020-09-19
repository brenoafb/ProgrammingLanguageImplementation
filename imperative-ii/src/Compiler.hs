module Compiler where

import Parser
import Machine

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

-- a VarTable contains an assignment of each variable used in the function
-- to a register index in the machine.
-- Each function has its VarTable
type VarTable = Map.Map String Int
type VarTables = Map.Map String VarTable
type Error = String
type PPCode = [Either String OP]

type CompileT a = ExceptT Error (Reader VarTable) a

compileFunction' :: Function -> PPCode
compileFunction' f =
  let varTable = buildVarTable f
      Right ppCode = runReader (runExceptT (compileFunction f)) varTable
   in ppCode

-- Left <function> indicates a function call
compileFunction :: Function -> CompileT PPCode
compileFunction (Function name args body) = compileStmt body

compileStmt :: Stmt -> CompileT PPCode
compileStmt (Assignment v e) = do
  c <- compileExpr e
  i <- lookupVar v
  return $ c ++ [Right $ STORE i]

compileStmt (If e s) = do
  ce <- compileExpr e
  cs <- compileStmt s
  return $ ce ++ [Right $ BZ (length cs)] ++ cs

compileStmt (IfElse e s1 s2) = do
  ce <- compileExpr e
  cs1 <- compileStmt s1
  cs2 <- compileStmt s2
  return $ ce
         ++ [Right $ BZ (length cs1)]
         ++ cs1
         ++ [Right $ BZ (length cs2)]
         ++ cs2

compileStmt (Block []) = return []
compileStmt (Block (stmt:stmts)) =
  (++) <$> compileStmt stmt <*> compileStmt (Block stmts)

compileStmt (While e s) = do
  ce <- compileExpr e
  cs <- compileStmt s
  let d1 = 2 + length cs
      d2 = negate $ 1 + length cs + length ce
  return $ ce ++ [Right $ BZ d1] ++ cs ++ [Right $ GOTO d2]

-- compileStmt (Return e) =
--   (++) <$> compileExpr e <*> restoreRegs
compileStmt (Return e) = do
  c <- compileExpr e
  return $ Right (STORE 0) : c
        ++ [Right $ LOAD 0, Right STOREPC]

compileExpr :: Expr -> CompileT PPCode

compileExpr (Num x) = return [Right $ PUSH x]

compileExpr (Var s) = do
  i <- lookupVar s
  return [Right $ LOAD i]

compileExpr (Neg e) = (++ [Right NEG]) <$> compileExpr e

compileExpr (FunCall f args) = do
  cArgs <- concat <$> traverse compileExpr args
  s <- saveRegs
  r <- restoreRegs
  return $ s
        ++ cArgs
        ++ [Right LOADPC]
        ++ [Left f]
        ++ [Right $ STORE 0] -- set return value aside
        ++ r
        ++ [Right $ LOAD 0] -- push retval back onto stack

-- arithmetic expressions
compileExpr e =
  (++ [Right op]) <$> ((++) <$> (compileExpr $ e1 e) <*> (compileExpr $ e2 e))
    where op = getOp e

lookupVar :: String -> CompileT Int
lookupVar v = do
  t <- ask
  case Map.lookup v t of
    Nothing -> throwError $ "Unassigned variable " ++ v
    Just i -> return i

saveRegs :: CompileT PPCode
saveRegs = do
  t <- ask
  let regs = filter (/= 0) $ Map.elems t -- by convention, temporary register is not saved
  return $ map (Right . LOAD) regs

restoreRegs :: CompileT PPCode
restoreRegs = do
  t <- ask
  let regs = filter (/= 0) $ Map.elems t
  return $ map (Right . STORE) regs

buildVarTables :: Program -> VarTables
buildVarTables fs = Map.fromList (zip names (map buildVarTable fs))
  where names = map (\(Function name _ _) -> name) fs

-- allocates found variables to registers 1 .. n
-- and keeps 0 as an extra/temporary register
buildVarTable :: Function -> VarTable
buildVarTable f = Map.fromList $ ("", 0) : zip (getVariables f) [1..]

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
