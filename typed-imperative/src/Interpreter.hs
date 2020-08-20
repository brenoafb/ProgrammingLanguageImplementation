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
           deriving (Eq, Show)

type Frame = M.Map String Value
type Env = [Frame]

type Error = String

type Eval a = ExceptT Error (ReaderT Program (State Env)) a

runProgram :: Program -> Env
runProgram program = execState (runReaderT (runExceptT exec) program) env
  where env = [M.empty]

getMainFunc :: Eval Function
getMainFunc = do
  functions <- ask
  case filter (\(Function name args retType body) -> name == "main") functions of
    [main] -> return main
    _ -> throwError "Invalid program: no main function"

exec :: Eval Env
exec = do
  (Function _ _ _ mainBody) <- getMainFunc
  execStmt mainBody

execStmt :: Stmt -> Eval Env
execStmt (Decl typ var) = undefined

execStmt (Assn var expr) = undefined

execStmt (If cond conseq) = undefined

execStmt (IfElse cond conseq alt) = undefined

execStmt (Block stmts) = undefined

execStmt (While cond body) = undefined

execStmt (Return expr) = undefined

eval :: Expr -> Eval Value
eval = undefined

