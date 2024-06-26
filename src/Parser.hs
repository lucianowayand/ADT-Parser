{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser (parseExpr, parseLiteral) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Types
import Data.Char (isUpper, isLower)

lingDef =
  emptyDef
    { Tok.reservedNames = ["in", "let", "then", "else", "case", "of", "True", "False"]
    }

lexer = Tok.makeTokenParser lingDef

identifier = Tok.identifier lexer

reserved = Tok.reserved lexer

reservedOp = Tok.reservedOp lexer

parens = Tok.parens lexer

integer = Tok.integer lexer

whiteSpace = Tok.whiteSpace lexer

symbol = Tok.symbol lexer

-- Parsing de Literais
parseLiteral :: Parser Literal
parseLiteral = do
  i <- integer
  return (LitInt i)
  <|> do
    reserved "True"
    return (LitBool True)
  <|> do
    reserved "False"
    return (LitBool False)

-- Parsing de Expressões
parseExpr :: Parser Expr
parseExpr =
  try parseLam
    <|> try parseLet
    <|> try parseIf
    <|> try parseCase
    <|> try parseApp
    <|> try parseTuple
    <|> try parseLit
    <|> try parseCons
    <|> parseVar
    <|> parens parseExpr

parseCons :: Parser Expr
parseCons = do
  x <- identifier
  if (not . null) x && isUpper (head x)
    then return (Const x)
    else parserFail "Expected a constructor"

parseVar :: Parser Expr
parseVar = do
  x <- identifier
  if (not . null) x && isLower (head x)
    then return (Var x)
    else parserFail "Expected a variable"

parseLam :: Parser Expr
parseLam = do
  reservedOp "\\"
  x <- identifier
  reservedOp "."
  e <- parseExpr
  return (Lam x e)

parseLet :: Parser Expr
parseLet = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- parseExpr
  reserved "in"
  e2 <- parseExpr
  return (Let (x, e1) e2)

parseIf :: Parser Expr
parseIf = do
  reserved "if"
  e1 <- parseExpr
  reserved "then"
  e2 <- parseExpr
  reserved "else"
  e3 <- parseExpr
  return (If e1 e2 e3)

parseCase :: Parser Expr
parseCase = do
  reserved "case"
  e <- parseExpr
  reserved "of"
  symbol "{"
  alts <- parseAlts
  symbol "}"
  return (Case e alts)

parseAlts :: Parser [(Pat, Expr)]
parseAlts = parseAlt `sepBy` reservedOp ";"

parseAlt :: Parser (Pat, Expr)
parseAlt = do
  p <- parsePat
  reservedOp "->"
  e <- parseExpr
  return (p, e)

parsePat :: Parser Pat
parsePat = try parsePLit <|> try parsePVar <|> try parsePCon

parsePVar :: Parser Pat
parsePVar = do
  x <- identifier
  return (PVar x)

parsePLit :: Parser Pat
parsePLit = do
  l <- parseLiteral
  return (PLit l)

parsePCon :: Parser Pat
parsePCon = do
  con <- identifier
  pats <- many parsePat
  return (PCon con pats)

-- Parsing de Tuplas
parseTuple :: Parser Expr
parseTuple = do
  symbol "("
  e1 <- parseExpr
  symbol ","
  e2 <- parseExpr
  symbol ")"
  return (Tuple e1 e2)

parseApp :: Parser Expr
parseApp = do
  es <- many1 parseTerm
  return (foldl1 App es)

parseTerm :: Parser Expr
parseTerm = do
  try parseVar
    <|> try parseLit
    <|> try parseTuple
    <|> parens parseExpr

parseLit :: Parser Expr
parseLit = do
  l <- parseLiteral
  return (Lit l)
