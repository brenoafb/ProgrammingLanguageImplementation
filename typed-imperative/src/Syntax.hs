{-# LANGUAGE DeriveDataTypeable #-}

module Syntax where

import Data.Generics

type Program = [Function]

data Function = Function String [(Type, String)] Type Stmt
  deriving (Show, Data, Typeable)

data Type = IntT
          | StrT
          | BoolT
          | VoidT
          deriving (Eq, Show, Data, Typeable)

data Expr = Num Int
          | Var String
          | Str String
          | ETrue
          | EFalse
          | FunCall String [Expr]
          | RelOp { relOp :: RelOp, e1 :: Expr, e2 :: Expr}
          | UnOp  { unOp :: UnOp, e1 :: Expr }
          | BinOp { binOp :: BinOp, e1 :: Expr, e2 :: Expr}
          deriving (Show, Data, Typeable)

data UnOp = Neg | Not
  deriving (Eq, Show, Data, Typeable)

data RelOp = Eq | NEq | Gt | Lt | LtEq | GtEq
  deriving (Eq, Show, Data, Typeable)

data BinOp = Add
           | Sub
           | Mult
           | Div
           | And
           | Or
           | Concat
  deriving (Eq, Show, Data, Typeable)

data Stmt = Decl Type String
          | Assn String Expr
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | Block [Stmt]
          | While Expr Stmt
          | Return Expr
          deriving (Show, Data, Typeable)
