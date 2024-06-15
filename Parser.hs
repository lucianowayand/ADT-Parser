module Parser (parseExpr, parseLiteral) where
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok
import Types
lingDef = emptyDef {
    Tok.reservedNames = ["in", "let", "then", "else"]
}
lexer = Tok.makeTokenParser lingDef

identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer
integer = Tok.integer lexer
whiteSpace = Tok.whiteSpace lexer

-- Parsing de Literais
parseLiteral :: Parser Literal
parseLiteral = (LitInt <$> integer) <|> (LitBool True <$ reserved "True") <|> (LitBool False <$ reserved "False")

-- Parsing de Expressões
parseExpr :: Parser Expr
parseExpr = parseLam
         <|> parseLet
         <|> parseIf
         <|> parseCase
         <|> parseApp
         <|> parseLit
         <|> parseVar
         <|> parens parseExpr

parseVar :: Parser Expr
parseVar = Var <$> identifier

parseLam :: Parser Expr
parseLam = do
    reservedOp "\\"
    x <- identifier
    reservedOp "."
    e <- parseExpr
    return $ Lam x e

parseLet :: Parser Expr
parseLet = do
    reserved "let"
    x <- identifier
    reservedOp "="
    e1 <- parseExpr
    reserved "in"
    e2 <- parseExpr
    return $ Let (x, e1) e2

parseIf :: Parser Expr
parseIf = do
    reserved "if"
    e1 <- parseExpr
    reserved "then"
    e2 <- parseExpr
    reserved "else"
    e3 <- parseExpr
    return $ If e1 e2 e3

parseCase :: Parser Expr
parseCase = do
    reserved "case"
    e <- parseExpr
    reserved "of"
    alts <- parseAlts
    return $ Case e alts

parseAlts :: Parser [(Pat, Expr)]
parseAlts = parseAlt `sepBy` reservedOp ";"

parseAlt :: Parser (Pat, Expr)
parseAlt = do
    p <- parsePat
    reservedOp "->"
    e <- parseExpr
    return (p, e)

parsePat :: Parser Pat
parsePat = parsePVar <|> parsePLit <|> parsePCon

parsePVar :: Parser Pat
parsePVar = PVar <$> identifier

parsePLit :: Parser Pat
parsePLit = PLit <$> parseLiteral

parsePCon :: Parser Pat
parsePCon = do
    con <- identifier
    pats <- many parsePat
    return $ PCon con pats

parseApp :: Parser Expr
parseApp = do
    es <- many1 parseTerm
    return $ foldl1 App es

parseTerm :: Parser Expr
parseTerm = parseVar <|> parseLit <|> parens parseExpr

parseLit :: Parser Expr
parseLit = Lit <$> parseLiteral
