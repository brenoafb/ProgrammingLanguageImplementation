module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expr = Num Integer
          | Var String
          | Let [(String,Expr)] Expr
          | Neg Expr
          | Add {e1 :: Expr, e2 :: Expr}
          | Sub {e1 :: Expr, e2 :: Expr}
          | Mult {e1 :: Expr, e2 :: Expr}
          | Div {e1 :: Expr, e2 :: Expr}
          deriving Show

languageDef = emptyDef { Token.identStart = letter
                       , Token.identLetter = alphaNum
                       , Token.reservedNames = [ "let"
                                               , "in"
                                               ]
                       , Token.reservedOpNames = [ "+", "-", "*", "/", "="]
                       }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
integer = Token.integer lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer

program :: Parser Expr
program = whiteSpace >> expr

expr :: Parser Expr
expr = try letExpr <|> buildExpressionParser operators term

letExpr :: Parser Expr
letExpr = do
  reserved "let"
  assingments <- sepBy1 assignment comma
  reserved "in"
  e2 <- expr
  return $ Let assingments e2

assignment :: Parser (String,Expr)
assignment = do
  var <- identifier
  reservedOp <- reserved "="
  e1 <- expr
  return (var, e1)

operators = [ [Prefix (reservedOp "-" >> return Neg)]
            , [Infix  (reservedOp "*" >> return Mult) AssocLeft,
               Infix  (reservedOp "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp "+" >> return Add) AssocLeft,
               Infix  (reservedOp "-" >> return Sub) AssocLeft]
            ]

term = parens expr
     <|>  Var <$> identifier
     <|> Num <$> integer

parseString :: String -> Expr
parseString str = case parse program "" str of
                    Left e -> error $ show e
                    Right r -> r
