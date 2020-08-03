module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expr = Mult Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Neg Expr
          | Num Integer
          deriving Show

languageDef = emptyDef { Token.reservedOpNames = ["+", "-", "*", "/"] }

lexer = Token.makeTokenParser languageDef

reservedOp = Token.reservedOp lexer
integer = Token.integer lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer

program :: Parser Expr
program = whiteSpace >> expr

expr :: Parser Expr
expr = buildExpressionParser operators term

operators = [ [Prefix (reservedOp "-" >> return Neg)]
            , [Infix  (reservedOp "*" >> return Mult) AssocLeft,
               Infix  (reservedOp "/" >> return Div) AssocLeft]
            , [Infix  (reservedOp "+" >> return Add) AssocLeft,
               Infix  (reservedOp "-" >> return Sub) AssocLeft]
            ]

term = parens expr
     <|> Num <$> integer

parseString :: String -> Expr
parseString str = case parse program "" str of
                    Left e -> error $ show e
                    Right r -> r
