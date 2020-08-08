module Interpreter where

import Prelude hiding (lookup)
import Parser
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

type Frame = Map.Map String Int
type Env = [Frame]

exec :: Stmt -> State Env Env
exec stmt = case stmt of
              Assignment var expr -> do
                env <- get
                let result = runReader (eval expr) env
                modify (update var result)
                get
              If cond conseq -> do
                env <- get
                let test = runReader (eval cond) env
                if test /= 0
                   then exec conseq
                   else return env
              IfElse cond conseq alt -> do
                env <- get
                let test = runReader (eval cond) env
                if test /= 0
                   then exec conseq
                   else exec alt
              Block (stmt:stmts) -> do
                env <- get
                modify (execState (exec stmt))
                exec (Block stmts)
              Block [] -> get
              While cond body -> do
                env <- get
                let test = runReader (eval cond) env
                if test /= 0
                   then exec $ Block [body, While cond body]
                   else return env
              Return exp -> undefined

eval :: Expr -> Reader Env Int
eval expr = case expr of
              Num x -> return x
              Var v -> lookup v
              FunCall func args -> undefined
              Neg e -> do
                x <- eval e
                return $ negate x
              _     -> do
                x <- eval (e1 expr)
                y <- eval (e2 expr)
                return $ getOp expr x y

lookup :: String -> Reader Env Int
lookup s = do
  env <- ask
  case env of
    [] -> error $ "lookup: unknown variable " ++ s
    (x:xs) -> case Map.lookup s x of
                Nothing -> error $ "lookup: unknown variable " ++ s
                Just v -> return v

-- Add or update entry (var,val) into the environment.
-- Entry can only added to the topmost frame
update :: String -> Int -> Env -> Env
update var val [] = error "update: Invalid environment"
update var val (env:envs) = case Map.lookup var env of
                              Just y -> Map.update f var env : envs
                                where f x = if x == y then Just val else Nothing
                              Nothing -> Map.insert var val env:envs

getOp :: Expr -> (Int -> Int -> Int)
getOp (Add _ _) = (+)
getOp (Sub _ _) = (-)
getOp (Mult _ _) = (*)
getOp (Div _ _) = div
