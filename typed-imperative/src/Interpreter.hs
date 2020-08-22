module Interpreter
  ( runProgram
  ) where

import Parser
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

data Value = IntV Int
           | StrV String
           | BoolV Bool
           | VoidV
           deriving (Eq, Show)

type Frame = M.Map String Value
type Env = [Frame]

type Error = String

type Eval a = ExceptT Error (ReaderT Program (State Env)) a

runProgram :: Program -> Either Error Env
runProgram program = evalState (runReaderT (runExceptT exec) program) env
  where env = [M.empty]

funcLookup :: String -> Eval Function
funcLookup name = do
  functions <- ask
  case filter (\(Function name' args retType body) -> name == name') functions of
    [func] -> return func
    _ -> throwError $ "Could not find function " ++ name

exec :: Eval Env
exec = do
  (Function _ _ _ mainBody) <- funcLookup "main"
  execStmt mainBody

execStmt :: Stmt -> Eval Env
execStmt (Decl typ var) = do
  modify (insertVar var typ)
  get

execStmt (Assn var expr) = do
  env <- get
  case env of
    (frame:_) -> if M.member var frame
      then do
        val <- eval expr
        modify $ \(frame:frames) -> M.insert var val frame : frames
        get
      else throwError $ "Undeclared variable " ++ var
    [] -> throwError "Invalid environment: empty stack"

execStmt (If cond conseq) = do
  condVal <- eval cond
  case condVal of
    (BoolV True) -> execStmt conseq
    (BoolV False) -> get
    _ -> throwError "Invalid if condition"

execStmt (IfElse cond conseq alt) = do
  condVal <- eval cond
  case condVal of
    (BoolV True) -> execStmt conseq
    (BoolV False) -> execStmt alt
    _ -> throwError "Invalid if-else condition"

execStmt (Block []) = get
execStmt (Block (stmt:stmts)) = do
  execStmt stmt
  execStmt (Block stmts)

execStmt (While cond body) = do
  condVal <- eval cond
  case condVal of
    (BoolV True) -> do
      execStmt body
      execStmt (While cond body)
    (BoolV False) -> get
    _ -> throwError "Invalid while condition"

execStmt (Return expr) = do
  env <- get
  case env of
    (frame:_) -> do
      val <- eval expr
      modify $ \(frame:frames) -> M.insert "__retval__" val frame : frames
      get
    [] -> throwError "return: empty environment"

eval :: Expr -> Eval Value
eval (Num x) = return $ IntV x
eval (Str s) = return $ StrV s
eval ETrue = return $ BoolV True
eval EFalse = return $ BoolV False

eval (Var var) = do
  env <- get
  case env of
    (frame:_) -> case M.lookup var frame of
      Nothing -> throwError $ "Undeclared variable " ++ var
      Just x -> return x
    [] -> throwError "Empty stack"

eval (FunCall name args) = do
  (Function _ bindings retType body) <- funcLookup name
  evaldArgs <- mapM eval args
  env <- get
  let argNames = map snd bindings
      newFrame = M.fromList $ zip argNames evaldArgs
  modify (newFrame :) -- push new stack frame
  if retType == VoidT
     then do
       execStmt body
       modify tail -- pop off stack frame
       return VoidV
     else do
       execStmt body
       env <- get
       case M.lookup "__retval__" (head env) of
         Nothing -> throwError "Error: non void function did not return"
         Just x  -> do
           modify tail -- pop off stack frame
           return x

eval (RelOp op e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (ev1, ev2) of
    (IntV x1, IntV x2) -> return $ relOpFunc op (IntV x1) (IntV x2)
    (BoolV b1, BoolV b2) | op `elem` [Eq, NEq] -> return $ relOpFunc op (BoolV b1) (BoolV b2)
    (StrV s1, StrV s2) | op `elem` [Eq, NEq] -> return $ relOpFunc op (StrV s1) (StrV s2)
    _ -> throwError "Invalid comparison operation"

eval (UnOp op e1) = do
  ev1 <- eval e1
  case (op, ev1) of
    (Neg, IntV x1) -> return . IntV $ negate x1
    (Not, BoolV b1) -> return . BoolV $ not b1
    _ -> throwError "Invalid unary operation"

eval (BinOp op e1 e2) = do
  ev1 <- eval e1
  ev2 <- eval e2
  case (op, ev1, ev2) of
    (op, IntV x1, IntV x2) | arithOp op -> return $ binOpFunc op (IntV x1) (IntV x2)
    (op, BoolV b1, BoolV b2) | logicalOp op -> return $ binOpFunc op (BoolV b1) (BoolV b2)
    (op, StrV s1, StrV s2) | strOp op -> return $ binOpFunc op (StrV s1) (StrV s2)
    _ -> throwError "Invalid binary operation"

insertVar :: String -> Type -> Env -> Env
insertVar var typ (frame:frames) = M.insert var (defaultVal typ) frame : frames

defaultVal :: Type -> Value
defaultVal IntT = IntV 0
defaultVal BoolT = BoolV False
defaultVal StrT = StrV ""
defaultVal VoidT = VoidV

arithOp :: BinOp -> Bool
arithOp op
  | op `elem` [Add, Sub, Mult, Div] = True
  | otherwise = False

logicalOp :: BinOp -> Bool
logicalOp op
  | op `elem` [And, Or] = True
  | otherwise = False

strOp :: BinOp -> Bool
strOp Concat = True
strOp _ = False

relOpFunc :: RelOp -> (Value -> Value -> Value)
relOpFunc Eq (IntV x1) (IntV x2) = BoolV $ x1 == x2
relOpFunc NEq (IntV x1) (IntV x2) = BoolV $ x1 /= x2
relOpFunc Lt (IntV x1) (IntV x2) = BoolV $ x1 < x2
relOpFunc Gt (IntV x1) (IntV x2) = BoolV $ x1 > x2
relOpFunc LtEq (IntV x1) (IntV x2) = BoolV $ x1 <= x2
relOpFunc GtEq (IntV x1) (IntV x2) = BoolV $ x1 >= x2
relOpFunc Eq (BoolV b1) (BoolV b2) = BoolV $ b1 == b2
relOpFunc NEq (BoolV b1) (BoolV b2) = BoolV $ b1 /= b2
relOpFunc Eq (StrV s1) (StrV s2) = BoolV $ s1 == s2
relOpFunc NEq (StrV s1) (StrV s2) = BoolV $ s1 /= s2

binOpFunc :: BinOp -> (Value -> Value -> Value)
binOpFunc Add (IntV x1) (IntV x2) = IntV $ x1 + x2
binOpFunc Sub (IntV x1) (IntV x2) = IntV $ x1 - x2
binOpFunc Mult (IntV x1) (IntV x2) = IntV $ x1 * x2
binOpFunc Div (IntV x1) (IntV x2) = IntV $ x1 `div` x2
binOpFunc And (BoolV x1) (BoolV x2) = BoolV $ x1 && x2
binOpFunc Or (BoolV x1) (BoolV x2) = BoolV $ x1 || x2
binOpFunc Concat (StrV x1) (StrV x2) = StrV $ x1 ++ x2
