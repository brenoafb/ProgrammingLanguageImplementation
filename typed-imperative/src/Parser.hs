module Parser where

import Syntax
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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
                                     , "int"
                                     , "str"
                                     , "bool"
                                     , "void"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/"
                                     , "=", "++", "&&", "||"
                                     , "!", ">", "<", "=="
                                     , ">=", "<=", "!=", ":"
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
stringLiteral = Token.stringLiteral lexer

parseStr :: String -> Program
parseStr str = case parse program "" str of
                 Left e -> error $ show e
                 Right r -> r

program :: Parser Program
program = whiteSpace >> many function

function :: Parser Function
function = do
  reserved "func"
  name <- identifier
  args <- parens (typedId `sepBy` comma)
  reservedOp ":"
  retType <- parseType
  body <- statement
  return $ Function name args retType body
    where typedId = do
            typ <- parseType
            idt <- identifier
            return (typ, idt)

statement :: Parser Stmt
statement = block
         <|> returnStmt
         <|> declaration
         <|> assignment
         <|> while
         <|> try ifElseStmt
         <|> ifStmt

block :: Parser Stmt
block = do
  stmts <- braces (many statement)
  case length stmts of
    1 -> return $ head stmts
    _ -> return $ Block stmts

declaration :: Parser Stmt
declaration = do
  typ <- parseType
  var <- identifier
  semi
  return $ Decl typ var

parseType :: Parser Type
parseType = (reserved "int" >> return IntT)
          <|> (reserved "str" >> return StrT)
          <|> (reserved "bool" >> return BoolT)
          <|> (reserved "void" >> return VoidT)

assignment :: Parser Stmt
assignment = do
    var <- identifier
    reservedOp "="
    e <- expr
    semi
    return $ Assn var e

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

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  e <- expr
  semi
  return $ Return e

expr :: Parser Expr
expr = buildExpressionParser ops term

ops = [ [Prefix (reservedOp "!" >> return (UnOp Not))]
      , [Infix (reservedOp "&&" >> return (BinOp And)) AssocLeft]
      , [Infix (reservedOp "||" >> return (BinOp Or))  AssocLeft]
      , [Infix (reservedOp "++" >> return (BinOp Concat)) AssocLeft]
      , [Prefix (reservedOp "-" >> return (UnOp Neg))]
      , [Infix  (reservedOp "*" >> return (BinOp Mult)) AssocLeft,
         Infix  (reservedOp "/" >> return (BinOp Div)) AssocLeft
        ]
      , [Infix  (reservedOp "+" >> return (BinOp Add)) AssocLeft,
         Infix  (reservedOp "-" >> return (BinOp Sub)) AssocLeft
        ]
      , [Infix  (reservedOp "==" >> return (RelOp Eq)) AssocLeft,
         Infix  (reservedOp "!=" >> return (RelOp NEq)) AssocLeft
        ]
      , [Infix  (reservedOp ">=" >> return (RelOp GtEq)) AssocLeft,
         Infix  (reservedOp "<=" >> return (RelOp LtEq)) AssocLeft
        ]
      , [Infix  (reservedOp ">" >> return (RelOp Gt)) AssocLeft,
         Infix  (reservedOp "<" >> return (RelOp Lt)) AssocLeft
        ]
      ]

funCall :: Parser Expr
funCall = do
  funName <- identifier
  args <- parens (expr `sepBy` comma)
  return $ FunCall funName args

term = parens expr
     <|> try funCall
     <|> (reserved "true" >> return ETrue)
     <|> (reserved "false" >> return EFalse)
     <|> Var <$> identifier
     <|> Num <$> int
     <|> Str <$> stringLiteral
