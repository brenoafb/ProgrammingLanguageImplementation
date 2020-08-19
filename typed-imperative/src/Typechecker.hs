module Typechecker where

import Parser
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M

type TypeC a = ReaderT String (State TypeEnv) a

type TypeEnv = M.Map String TypeInfo

data TypeInfo = VarT { varType :: Type }
              | FuncT { argTypes :: [Type], retType :: Type }
              deriving (Eq, Show)

-- populate initial environment with functions
typecheck :: Program -> Bool
typecheck funcs = let env = M.fromList (map funcTypeInfo funcs)
                   in go funcs env
                  where go [] env = True
                        go (func@(Function name _ _ _):funcs) env =
                          case runState (runReaderT (typecheckFunc func) name) env of
                            (False, _) -> False
                            (True, env') -> go funcs env'

funcTypeInfo :: Function -> (String, TypeInfo)
funcTypeInfo (Function name args retType body) = (name, FuncT argTypes retType)
  where argTypes = map fst args

typecheckFunc :: Function -> TypeC Bool
typecheckFunc (Function name args retType body) =
  typecheckStmt body

typecheckStmt :: Stmt -> TypeC Bool
typecheckStmt (Decl typ var) = do
  env <- get
  case M.lookup var env of
    Nothing -> do
      modify $ M.insert var (VarT typ)
      return True
    Just _ -> return False -- redeclaration of variable

typecheckStmt (Assn var e) = do
  env <- get
  case M.lookup var env of
    Nothing -> return False -- variable not declared
    Just (VarT typ) -> do
      expType <- getType e
      case expType of
        Just (VarT typ') -> return $ typ' == typ
        _ -> return False
    Just _ -> return False -- assigning value to function

typecheckStmt (If cond body) = do
  condType <- getType cond
  case condType of
    Just (VarT BoolT) -> typecheckStmt body
    _ -> return False -- cond is not Bool

typecheckStmt (IfElse cond conseq alt) = do
  condType <- getType cond
  case condType of
    Just (VarT BoolT) -> (&&) <$> typecheckStmt conseq <*> typecheckStmt alt
    _ -> return False -- cond is not Bool

typecheckStmt (Block stmts) = foldM (\acc stmt -> (acc &&) <$> typecheckStmt stmt) True stmts

typecheckStmt (While cond body) = do
  condType <- getType cond
  case condType of
    Just (VarT BoolT) -> typecheckStmt body
    _ -> return False -- cond is not Bool

typecheckStmt (Return e) = do
  funcName <- ask
  env <- get
  case M.lookup funcName env of
    Just (FuncT _ retType) -> do
      exprTypeM <- getType e
      case exprTypeM of
        Just (VarT exprType) | exprType == retType -> return True
        _ -> return False
    _ -> return False

-- Returns 'Nothing' if the expression does not typecheck.
-- Return Just <type of expression> otherwise
getType :: Expr -> TypeC (Maybe TypeInfo)
getType (Num _) = return $ Just (VarT IntT)
getType (Var s) = gets $ M.lookup s
getType (Str s) = return $ Just (VarT StrT)
getType ETrue = return $ Just (VarT BoolT)
getType EFalse = return $ Just (VarT BoolT)
getType (FunCall s args) = do
  env <- get
  case M.lookup s env of
    Just (FuncT argTypes retType) -> do -- argTypes :: [Type]
      types <- mapM getType args -- types :: [Maybe TypeInfo]
      -- sequence types :: Maybe [TypeInfo]
      case sequence types of
        Just typeInfos -> if map VarT argTypes == typeInfos -- typeInfos :: [TypeInfo]
                          then return $ Just (VarT retType)
                          else return Nothing -- arg types don't match
        _ -> return Nothing
    _ -> return Nothing
getType (RelOp _ e1 e2) = do
  typ1 <- getType e1
  typ2 <- getType e2
  case typ1 of
    Just (VarT IntT) -> case typ2 of
      Just (VarT IntT) -> return $ Just (VarT BoolT)
      _ -> return Nothing
    _ -> return Nothing
getType (UnOp op e1) = typecheckUnOp op e1
getType (BinOp op e1 e2) = typecheckBinOp op e1 e2

typecheckUnOp :: UnOp -> Expr -> TypeC (Maybe TypeInfo)
typecheckUnOp op e1 = do
  typ1 <- getType e1
  let typ = unOpType op
  case typ1 of
    Just (VarT typ') | typ' == typ -> return $ Just $ VarT typ
    _ -> return Nothing

typecheckBinOp :: BinOp -> Expr -> Expr -> TypeC (Maybe TypeInfo)
typecheckBinOp op e1 e2 = do
  typ1 <- getType e1
  typ2 <- getType e2
  let typ = binOpType op
  case typ1 of
    Just (VarT typ') | typ' == typ -> case typ2 of
      Just (VarT typ'') | typ'' == typ -> return $ Just $ VarT typ
      _ -> return Nothing
    _ -> return Nothing

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
