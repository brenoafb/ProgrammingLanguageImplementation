module Typechecker
  ( typecheck
  ) where

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
typecheck :: Program -> Maybe String
typecheck funcs = go funcs
  where env = M.fromList (map funcTypeInfo funcs)
        go [] = Nothing
        go (func@(Function name args retType body):funcs) =
          let funcEnv = getFuncEnv env args
           in case evalState (runReaderT (typecheckFunc func) name) funcEnv of
            Just err -> Just err
            Nothing -> go funcs

getFuncEnv :: TypeEnv -> [(Type, String)] -> TypeEnv
getFuncEnv = foldr (\(typ, arg) acc -> M.insert arg (VarT typ) acc)


funcTypeInfo :: Function -> (String, TypeInfo)
funcTypeInfo (Function name args retType body) = (name, FuncT argTypes retType)
  where argTypes = map fst args

typecheckFunc :: Function -> TypeC (Maybe String)
typecheckFunc (Function name args retType body) =
  typecheckStmt body

typecheckStmt :: Stmt -> TypeC (Maybe String)
typecheckStmt (Decl typ var) = do
  env <- get
  case M.lookup var env of
    Nothing -> do
      modify $ M.insert var (VarT typ)
      return Nothing
    Just _ -> return $ Just $ "Redeclaring variable " ++ var

typecheckStmt (Assn var e) = do
  env <- get
  case M.lookup var env of
    Nothing -> return $ Just $ "Variable " ++ var ++ " assigned but not declared"
    Just (VarT typ) -> do
      expType <- getType e
      case expType of
        Right (VarT typ') -> if typ' == typ
                               then return Nothing
                               else return $ Just $ "Invalid assignment type: " ++ var
        Right (FuncT _ _) -> return $ Just $ "Cannot assign value to function " ++ var
        Left err -> return $ Just $ err ++ " -- var"
    Just _ -> return $ Just $ "Assigning value to function " ++ var -- assigning value to function

typecheckStmt (If cond body) = do
  condType <- getType cond
  case condType of
    Right (VarT BoolT) -> typecheckStmt body
    _ -> return $ Just "if condition is not bool"

typecheckStmt (IfElse cond conseq alt) = do
  condType <- getType cond
  case condType of
    Right (VarT BoolT) -> do
      conseqCheck <- typecheckStmt conseq
      case conseqCheck of
        Nothing -> typecheckStmt alt
        _ -> return conseqCheck
    Left err -> return $ Just $ "if-else: " ++ err

typecheckStmt (Block (stmt:stmts)) = do
  stmtCheck <- typecheckStmt stmt
  case stmtCheck of
    Nothing -> typecheckStmt $ Block stmts
    _ -> return stmtCheck
typecheckStmt (Block []) = return Nothing

typecheckStmt (While cond body) = do
  condType <- getType cond
  case condType of
    Right (VarT BoolT) -> typecheckStmt body
    Left err -> return $ Just $ "while: " ++ err

typecheckStmt (Return e) = do
  funcName <- ask
  env <- get
  case M.lookup funcName env of
    Just (FuncT _ retType) -> do
      exprTypeM <- getType e
      case exprTypeM of
        Right (VarT exprType) | exprType == retType -> return Nothing
        Right (VarT exprType) | exprType /= retType -> return $ Just "Invalid return"
        Left err -> return $ Just $ "return: " ++ err
    _ -> return $ Just "invalid return"

-- Returns 'Nothing' if the expression does not typecheck.
-- Return Just <type of expression> otherwise
getType :: Expr -> TypeC (Either String TypeInfo)
getType (Num _) = return $ Right (VarT IntT)
getType (Var s) = do
  env <- get
  case M.lookup s env of
    Nothing -> return $ Left $ "Undeclared variable " ++ s
    Just typ -> return $ Right typ

getType (Str s) = return $ Right (VarT StrT)
getType ETrue = return $ Right (VarT BoolT)
getType EFalse = return $ Right (VarT BoolT)
getType (FunCall s args) = do
  env <- get
  case M.lookup s env of
    Just (FuncT argTypes retType) -> do -- argTypes :: [Type]
      types <- mapM getType args -- types :: [Either String TypeInfo]
      -- sequence types :: Either String [TypeInfo]
      case sequence types of
        Right typeInfos -> if map VarT argTypes == typeInfos -- typeInfos :: [TypeInfo]
                          then return $ Right (VarT retType)
                          else return $ Left $ "invalid type in function call for " ++ s
        _ -> return $ Left $ "type error in function call for " ++ s
    _ -> return $ Left $ "type error in function call for " ++ s
getType (RelOp _ e1 e2) = do
  typ1 <- getType e1
  typ2 <- getType e2
  case typ1 of
    Right (VarT IntT) -> case typ2 of
      Right (VarT IntT) -> return $ Right (VarT BoolT)
      _ -> return $ Left "type error in relational expression"
    _ -> return $ Left "type error in relational expression"
getType (UnOp op e1) = typecheckUnOp op e1
getType (BinOp op e1 e2) = typecheckBinOp op e1 e2

typecheckUnOp :: UnOp -> Expr -> TypeC (Either String TypeInfo)
typecheckUnOp op e1 = do
  typ1 <- getType e1
  let typ = unOpType op
  case typ1 of
    Right (VarT typ') | typ' == typ -> return $ Right $ VarT typ
    Left err -> return $ Left $ "unary op error -- " ++ err

typecheckBinOp :: BinOp -> Expr -> Expr -> TypeC (Either String TypeInfo)
typecheckBinOp op e1 e2 = do
  typ1 <- getType e1
  typ2 <- getType e2
  let typ = binOpType op
  case typ1 of
    Right (VarT typ') | typ' == typ -> case typ2 of
      Right (VarT typ'') | typ'' == typ -> return $ Right $ VarT typ
      Left err -> return $ Left $ "binary op error -- " ++ err
    Right (VarT typ') | typ' /= typ -> return $ Left "binary op error -- invalid operator for expressions"
    Left err -> return $ Left $ "binary op error -- " ++ err
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
