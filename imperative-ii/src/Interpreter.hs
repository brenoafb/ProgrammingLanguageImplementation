module Interpreter where

import Prelude hiding (lookup)
import Parser
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

type Frame = Map.Map String Int
type Env = [Frame]

type Eval a = ReaderT Program (State Env) a

exec :: Program -> Env
exec functions =
  let (Function _ _ body) = head $ filter (\(Function n _ _) -> n == "main") functions
      env  = [Map.empty]
  in execState (runReaderT (execStmt body) functions) env

execStmt :: Stmt -> Eval Env
execStmt stmt = case stmt of
              Assignment var expr -> do
                result <- eval expr
                modify (update var result)
                get
              If cond conseq -> do
                test <- eval cond
                if test /= 0
                   then execStmt conseq
                   else get
              IfElse cond conseq alt -> do
                test <- eval cond
                if test /= 0
                   then execStmt conseq
                   else execStmt alt
              Block (stmt:stmts) -> do
                env <- get
                execStmt stmt
                execStmt (Block stmts)
              Block [] -> get
              While cond body -> do
                test <- eval cond
                if test /= 0
                   then execStmt $ Block [body, While cond body]
                   else get
              Return e -> do
                val <- eval e
                modify $ update "__retval__" val
                get

eval :: Expr -> Eval Int
eval expr = case expr of
              Num x -> return x
              Var v -> gets $ lookup v
              FunCall func args -> do
                functions <- ask
                case filter (\(Function name _ _) -> name == func) functions of
                  [] -> error $ "Unknown function " ++ func
                  [Function _ argNames body] -> do
                    evaldArgs <- traverse eval args
                    let bindings = Map.fromList $ zip argNames evaldArgs
                    modify (bindings :)
                    execStmt body
                    env <- get
                    case Map.lookup "__retval__" $ head env of
                          Nothing -> error $ "Function " ++ func ++ " did not return any value"
                          Just x -> do
                            modify tail
                            return x
                  _ -> error $ "Function " ++ func ++ " declared more than once."
              Neg e -> do
                x <- eval e
                return $ negate x
              _     -> do
                x <- eval (e1 expr)
                y <- eval (e2 expr)
                return $ getOp expr x y

-- WARNING: unsafe, fails if env does not contain variable
lookup :: String -> Env -> Int
lookup s env = case env of
    [] -> error $ "lookup: unknown variable " ++ s
    (x:xs) -> case Map.lookup s x of
                Nothing -> error $ "lookup: unknown variable " ++ s
                Just v -> v

-- Add or update entry (var,val) into the environment.
-- Entry can only added to the topmost frame
-- WARNING: unsafe, fails when the stack contains no frames
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
