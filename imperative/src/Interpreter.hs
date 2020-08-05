module Interpreter where

import Parser
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

type Env = Map.Map String Int

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
              While cond body -> undefined

eval :: Expr -> Reader Env Int
eval expr = case expr of
              Num x -> return x
              Var v -> do
                env <- ask
                case Map.lookup v env of
                  Nothing -> error "unknown variable"
                  Just x -> return x
              Neg e -> do
                x <- eval e
                return $ negate x
              _     -> do
                x <- eval (e1 expr)
                y <- eval (e2 expr)
                return $ getOp expr x y

update :: String -> Int -> Env -> Env
update v x env = case Map.lookup v env of
                   Nothing -> Map.insert v x env
                   Just _ -> Map.update f v env
  where f _ = Just x

getOp :: Expr -> (Int -> Int -> Int)
getOp (Add _ _) = (+)
getOp (Sub _ _) = (-)
getOp (Mult _ _) = (*)
getOp (Div _ _) = div
