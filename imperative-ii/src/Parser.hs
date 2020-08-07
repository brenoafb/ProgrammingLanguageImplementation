module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expr = Num Int
          | Var String
          | Neg Expr
          | FunCall String [Expr]
          | Add {e1 :: Expr, e2 :: Expr}
          | Sub {e1 :: Expr, e2 :: Expr}
          | Mult {e1 :: Expr, e2 :: Expr}
          | Div {e1 :: Expr, e2 :: Expr}
          deriving Show

data Stmt = Assignment String Expr
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | Block [Stmt]
          | While Expr Stmt
          | FunDec String [String] Stmt
          | Return Expr
          deriving Show

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "else"
                                     , "while"
                                     , "func"
                                     , "return"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/"
                                     , "="
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
int = fromIntegral <$> Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer

languageParser :: Parser [Stmt]
languageParser = whiteSpace >> many funDec

statement :: Parser Stmt
statement = block <|> assignment <|> ifElseStmt
          <|> ifStmt <|> while <|> funDec <|> returnStmt

block :: Parser Stmt
block = Block <$> braces (many statement)

assignment :: Parser Stmt
assignment = do
    var <- identifier
    reservedOp "="
    e <- expr
    semi
    return $ Assignment var e

ifElseStmt :: Parser Stmt
ifElseStmt = do
  reserved "if"
  cond <- parens expr
  conseq <- statement
  reserved "else"
  alt <- statement
  return $ IfElse cond conseq alt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- parens expr
  stmt <- statement
  return $ If cond stmt

while :: Parser Stmt
while = do
  reserved "while"
  cond <- parens expr
  body <- statement
  return $ While cond body

funDec :: Parser Stmt
funDec = do
  reserved "func"
  name <- identifier
  args <- parens (identifier `sepBy` comma)
  body <- statement
  return $ FunDec name args body


returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  e <- expr
  semi
  return $ Return e

expr :: Parser Expr
expr = try funCall <|> buildExpressionParser operators term

funCall :: Parser Expr
funCall = do
  funName <- identifier
  args <- parens (sepBy expr comma)
  return $ FunCall funName args

operators = [ [Prefix (reservedOp "-" >> return Neg)]
            , [Infix  (reservedOp "*" >> return Mult) AssocLeft,
               Infix  (reservedOp "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp "+" >> return Add) AssocLeft,
               Infix  (reservedOp "-" >> return Sub) AssocLeft]
            ]

term = parens expr
     <|>  Var <$> identifier
     <|> Num <$> int

parseStr :: String -> [Stmt]
parseStr str = case parse languageParser "" str of
                 Left e -> error $ show e
                 Right r -> r
