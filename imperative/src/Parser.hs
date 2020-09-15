module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expr = Num Int
          | Var String
          | Neg Expr
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

languageParser :: Parser Stmt
languageParser = whiteSpace >> statement

statement :: Parser Stmt
statement = block <|> assignment <|> ifElseStmt <|> ifStmt <|> while

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

expr :: Parser Expr
expr = buildExpressionParser operators term

operators = [ [Prefix (reservedOp "-" >> return Neg)]
            , [Infix  (reservedOp "*" >> return Mult) AssocLeft,
               Infix  (reservedOp "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp "+" >> return Add) AssocLeft,
               Infix  (reservedOp "-" >> return Sub) AssocLeft]
            ]

term = parens expr
     <|>  Var <$> identifier
     <|> Num <$> int

parseString :: String -> Either ParseError Stmt
parseString = parse (languageParser <* eof) ""
