module Interpreter where

import Parser
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M

data Value = IntV Int
           | StrV String
           | BoolV Bool
           deriving (Eq, Show)

type Frame = M.Map String Value
type Env = [Frame]

type Eval a = ReaderT Program (State Env) a

exec :: Program -> Env
exec = undefined

execStmt :: Stmt -> Eval Env
execStmt = undefined

eval :: Function -> Eval Value
eval = undefined

