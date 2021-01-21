module Typechecker
  ( typecheck
  ) where

import Syntax
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

type Error = String

type TypeC a = ExceptT Error (ReaderT String (State TypeEnv)) a

type TypeEnv = M.Map String TypeInfo

data TypeInfo = VarT { varType :: Type }
              | FuncT { argTypes :: [Type], retType :: Type }
              deriving (Eq, Show)

typecheck :: Program -> Maybe String
typecheck funcs = go funcs
  where env = M.fromList (map funcTypeInfo funcs)
        go [] = Nothing
        go (func@(Function name args retType body):funcs) =
          let funcEnv = getFuncEnv env args
           in case evalState (runReaderT (runExceptT $ typecheckFunc func) name) funcEnv of
            Left err -> Just err
            Right () -> Nothing

getFuncEnv :: TypeEnv -> [(Type, String)] -> TypeEnv
getFuncEnv = foldr (\(typ, arg) acc -> M.insert arg (VarT typ) acc)


funcTypeInfo :: Function -> (String, TypeInfo)
funcTypeInfo (Function name args retType body) = (name, FuncT argTypes retType)
  where argTypes = map fst args

typecheckFunc :: Function -> TypeC ()
typecheckFunc (Function name args retType body) =
  typecheckStmt body

typecheckStmt :: Stmt -> TypeC ()
typecheckStmt (Decl typ var) = do
  env <- get
  case M.lookup var env of
    Nothing -> do
      modify $ M.insert var (VarT typ)
      return ()
    Just _ -> throwError $ "Redeclaring variable " ++ var

typecheckStmt (Assn var e) = do
  env <- get
  case M.lookup var env of
    Nothing -> throwError $ "Variable " ++ var ++ " assigned but not declared"
    Just typ -> do
      expType <- getType e
      if expType == typ
         then return ()
         else throwError $ "Invalid assignment type for " ++ var

typecheckStmt (If cond conseq) = do
  condType <- getType cond
  case condType of
    VarT BoolT -> typecheckStmt conseq
    _ -> throwError $ "if error: invalid condition"

typecheckStmt (IfElse cond conseq alt) = do
  condType <- getType cond
  case condType of
    VarT BoolT -> do
      typecheckStmt conseq
      typecheckStmt alt
    _ -> throwError $ "if-else error: invalid condition"

typecheckStmt (Block (stmt:stmts)) = do
  typecheckStmt stmt
  typecheckStmt $ Block stmts
typecheckStmt (Block []) = return ()

typecheckStmt (While cond body) = do
  funcName <- ask
  condType <- getType cond
  case condType of
    VarT BoolT -> typecheckStmt body
    _ -> throwError $ "error in while statement condition: " ++ funcName

typecheckStmt (Return e) = do
  funcName <- ask
  env <- get
  case M.lookup funcName env of
    Just (FuncT _ retType) -> do
      exprType <- getType e
      case exprType of
        VarT exprType | exprType == retType -> return ()
        VarT exprType | exprType /= retType -> throwError $ "Return type error for " ++ funcName
    _ -> throwError $ "Invalid return: " ++ funcName

getType :: Expr -> TypeC TypeInfo
getType (Num _) = return $ VarT IntT
getType (Var s) = do
  env <- get
  case M.lookup s env of
    Nothing -> throwError $ "Undeclared variable " ++ s
    Just typ -> return typ

getType (Str s) = return $ VarT StrT
getType ETrue = return $ VarT BoolT
getType EFalse = return $ VarT BoolT
getType (FunCall s args) = do
  env <- get
  case M.lookup s env of
    Just (FuncT argTypes retType) -> do -- argTypes :: [Type]
      types <- mapM getType args -- types :: [TypeInfo]
      if map VarT argTypes == types
         then return $ VarT retType
         else throwError $ "Error in function call for " ++ s
getType (RelOp _ e1 e2) = do
  typ1 <- getType e1
  typ2 <- getType e2
  case (typ1, typ2) of
    (VarT IntT, VarT IntT) -> return $ VarT BoolT
getType (UnOp op e1) = typecheckUnOp op e1
getType (BinOp op e1 e2) = typecheckBinOp op e1 e2

typecheckUnOp :: UnOp -> Expr -> TypeC TypeInfo
typecheckUnOp op e1 = do
  typ1 <- getType e1
  let typ = unOpType op
  case typ1 of
    VarT typ' | typ' == typ -> return $ VarT typ
    _ -> throwError "unary op error"

typecheckBinOp :: BinOp -> Expr -> Expr -> TypeC TypeInfo
typecheckBinOp op e1 e2 = do
  typ1 <- getType e1
  typ2 <- getType e2
  let typ = binOpType op
  case (typ1, typ2) of
    (VarT typ1', VarT typ2') | (typ == typ1') && (typ == typ2') -> return $ VarT typ
    _ -> throwError "binary op error"

unOpType :: UnOp -> Type
unOpType Not = BoolT
unOpType Neg = IntT

binOpType :: BinOp -> Type
binOpType Add = IntT
binOpType Sub = IntT
binOpType Mult = IntT
binOpType Div = IntT
binOpType And = BoolT
binOpType Or = BoolT
binOpType Concat = StrT
