module Compiler
  ( compile
  , printASM
  ) where

import Prelude hiding (lookup)
import Parser
import Machine

import qualified Data.List as L
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

type AddressTable = Map.Map String Int

type CompileT r a = ExceptT Error (Reader r) a

printASM :: [OP] -> IO ()
printASM asm =
  mapM_ (\(i, c) -> putStrLn $ show i ++ "\t" ++ show c) $ zip [0..] asm

compile :: Program -> Either Error [OP]
compile p = do
  ppCodes <- orderFunctions p
  let t = buildAddressTable ppCodes
      x = concat <$> traverse h ppCodes
    in runReader (runExceptT x) t
   where h (f, pp) = traverse g pp
         g (Left s) = CALL <$> lookup s
         g (Right op) = pure op

orderFunctions :: Program -> Either Error [(String, PPCode)]
orderFunctions p =
  case L.partition (\(Function name _ _) -> name == "main") p of
    ([main], fs) -> do
      mainC <- (\c -> ("main", c)) <$> ((++ [Right HALT]) <$> compileFunction' main)
      fsC <- go fs
      return $ mainC : fsC
    _ -> throwError "Invalid program"
  where go [] = pure []
        go (f@(Function name _ _):fs) =
           (:) <$> ((\c -> (name,c)) <$> compileFunction' f)
               <*> go fs

buildAddressTable :: [(String, PPCode)] -> AddressTable
buildAddressTable = Map.fromList . g 0 . lengths
  where g i ((n, c):xs) = (n, i) : g (i + c) xs
        g i [] = []
        lengths = map (\(n,c) -> (n, length c))

compileFunction' :: Function -> Either Error PPCode
compileFunction' f =
  let varTable = buildVarTable f
   in runReader (runExceptT (compileFunction f)) varTable

-- Left <function> indicates a function call
compileFunction :: Function -> CompileT VarTable PPCode
-- compileFunction (Function name args body) =
--   (++) <$> storeArgs args <*> compileStmt body
compileFunction (Function "main" args body) = compileStmt body
compileFunction (Function name args body) = do
  s <- storeArgs args
  c <- compileStmt body
  return $ Right (STORE 0) : s ++ [Right $ LOAD 0] ++ c

compileStmt :: Stmt -> CompileT VarTable PPCode
compileStmt (Assignment v e) = do
  c <- compileExpr e
  i <- lookup v
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

compileStmt (Return e) = do
  c <- compileExpr e
  return $ Right (STORE 0) : c
        ++ [Right $ LOAD 0, Right STOREPC]

compileExpr :: Expr -> CompileT VarTable PPCode

compileExpr (Num x) = return [Right $ PUSH x]

compileExpr (Var s) = do
  i <- lookup s
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

lookup :: Ord k => k -> CompileT (Map.Map k v) v
lookup x = do
  m <- ask
  case Map.lookup x m of
    Nothing -> throwError "lookup: invalid key"
    Just y -> return y

saveRegs :: CompileT VarTable PPCode
saveRegs = do
  t <- ask
  let regs = Map.elems t
  return $ map (Right . LOAD) regs

restoreRegs :: CompileT VarTable PPCode
restoreRegs = do
  t <- ask
  let regs = reverse . filter (/= 0) $ Map.elems t
  return $ map (Right . STORE) regs

storeArgs :: [String] -> CompileT VarTable PPCode
storeArgs args = do
  t <- ask
  case traverse (`Map.lookup` t) args of
    Nothing -> throwError "Error obtaining function arguments"
    Just regs -> return $ map (Right . STORE) $ reverse regs

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
