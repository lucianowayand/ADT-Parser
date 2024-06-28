-- Alunos: Ian Halfen e Luciano Wayand


import Text.Parsec
import Type
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Data.Char (isUpper, isLower)
import Control.Monad (mapAndUnzipM)
import Data.Maybe (fromJust)

lingDef =
  emptyDef
    { Tok.reservedNames = ["in", "let", "if","then", "else", "case", "of", "True", "False"]
    }

lexer = Tok.makeTokenParser lingDef

identifier = Tok.identifier lexer

reserved = Tok.reserved lexer

reservedOp = Tok.reservedOp lexer

parens = Tok.parens lexer

integer = Tok.integer lexer

whiteSpace = Tok.whiteSpace lexer

symbol = Tok.symbol lexer

data Literal = LitInt Integer | LitBool Bool deriving (Eq, Show)

data Pat = PVar Id
         | PLit Literal
         | PCon Id [Pat]
         deriving (Eq, Show)


data Expr =   Var Id
            | App Expr Expr
            | Lam Id Expr
            | Lit Literal
            | If Expr Expr Expr
            | Cons Id
            | Case Expr [(Pat, Expr)]
            | Let (Id, Expr) Expr
            deriving (Eq, Show)

tiContext g i = if l /= [] then freshInst t else error ("Variavel " ++ i ++ " indefinida\n")
   where
      l = dropWhile (\(i' :>: _) -> i /= i' ) g 
      (_ :>: t) = head l

maxTGen (TApp t1 t2) = max (maxTGen t1) (maxTGen t2)
maxTGen (TArr t1 t2) = max (maxTGen t1) (maxTGen t2)
maxTGen (TGen n)     = n
maxTGen _            = -1

freshInst t = do ts <- mapM (\_ -> freshVar) [0..maxTGen t]
                 return (inst ts t)

inst ts (TApp l r) = TApp  (inst ts l) (inst ts r)
inst ts (TArr l r) = TArr (inst ts l) (inst ts r)
inst ts (TGen i)  = ts !! i
inst ts t         = t


tiExpr :: [Assump] -> Expr -> TI (SimpleType, Subst)
tiExpr g (Var i) = do{t <- tiContext g i; return (t, [])}
tiExpr g (App e e') = do (t, s1) <- tiExpr g e
                         (t', s2) <- tiExpr (apply s1 g) e'
                         b <- freshVar
                         let s3 = unify (apply s2 t) (t' --> b)
                         return (apply s3 b, s3 @@ s2 @@ s1)
tiExpr g (Lam i e) = do b <- freshVar
                        (t, s)  <- tiExpr (g/+/[i:>:b]) e
                        return (apply s (b --> t), s)
tiExpr g (Lit (LitInt i)) = return (TCon "Int", [])
tiExpr g (Lit (LitBool i)) = return (TCon "Bool", [])
tiExpr g (Cons c) = do{t <- tiContext g c; return (t, [])}
tiExpr g (If cond e1 e2) = do
    (tCond, s1) <- tiExpr g cond
    let s2 = unify tCond (TCon "Bool")  -- A condição deve ser booleana
    (t1, s3) <- tiExpr (apply s2 g) e1
    (t2, s4) <- tiExpr (apply s3 g) e2
    let s5 = unify (apply s4 t1) t2  -- Os tipos das ramificações devem ser iguais
    return (apply s5 t1, s5 @@ s4 @@ s3 @@ s2 @@ s1)
tiExpr g (Let (i, e1) e2) = do
    (t1, s1) <- tiExpr g e1
    (t2, s2) <- tiExpr (apply s1 (g /+/ [i :>: t1])) e2
    return (t2, s2 @@ s1)
tiExpr g (Case e alts) = do
    (t, s) <- tiExpr g e
    tAlt <- freshVar
    sAlts <- mapM (inferAlt t tAlt (apply s g)) alts
    let s' = foldr1 (@@) sAlts
    return (apply s' tAlt, s' @@ s)
  where
    inferAlt :: SimpleType -> SimpleType -> [Assump] -> (Pat, Expr) -> TI Subst
    inferAlt t tAlt g (pat, expr) = do
        (as, s1) <- tiPat pat
        (tExpr, s2) <- tiExpr (g /+/ as) expr
        let s3 = unify tAlt tExpr
            s4 = unify (apply s2 t) tAlt
        return (s4 @@ s3 @@ s2 @@ s1)


tiPat :: Pat -> TI ([Assump], Subst)
tiPat (PVar i) = do
    v <- freshVar
    return ([i :>: v], [])
tiPat (PLit (LitInt _)) = return ([], [])
tiPat (PLit (LitBool _)) = return ([], [])
tiPat (PCon c ps) = do
    v <- freshVar
    (assumps, substs) <- mapAndUnzipM tiPat ps
    let s = foldr1 (@@) substs
    return (concat assumps, s)


--- Examples ---
ex1 = Lam "f" (Lam "x" (App (Var "f") (Var "x"))) -- \f.\x.f x
ex2 = Lam "x" (App (Var "x") (Var "x")) -- \x.x x
ex3 = Lam "g" (Lam "f" (Lam "x" (App (Var "g") (App (Var "f") (Var "x")))))
ex4 = Lam "x" (Lam "x" (Var "x"))
ex5 = Lam "w" (Lam "y" (Lam "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x")))))
ex6 = Lam "x" (Lam "y" (Lam "w" (Lam "u" (App (App (Var "x") (Var "w")) (App (App (Var "y") (Var "w")) (Var "u"))))))

iniCont = ["(,)" :>: TArr (TGen 0) (TArr (TGen 1) (TArr (TArr (TCon "(,)") (TGen 0)) (TGen 1))),
            "True" :>: TCon "Bool", "False" :>: TCon "Bool",
            "Just" :>: TArr (TGen 0) (TArr (TCon "Maybe") (TGen 0)), "Nothing" :>: TArr (TCon "Maybe") (TGen 0)]


infer e = runTI (tiExpr iniCont e)

-------- Parser ---------------
parseExpr = runParser expr [] "lambda-calculus"

expr :: Parsec String u Expr
expr = chainl1 (between spaces spaces parseNonApp) $ return App

--parseLiteral :: Parser Literal
parseLiteral = do
  LitInt <$> integer
  <|> do
    reserved "True"
    return (LitBool True)
  <|> do
    reserved "False"
    return (LitBool False)

--parseLit :: Parser Expr
parseLit = do
  Lit <$> parseLiteral

--parseLet :: Parser Expr
parseLet = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let (x, e1) <$> expr

var x = return (Var x)

cons x = return (Cons x)

varOuCons = do
  x <- identifier
  if isLower (head x) then var x
  else cons x

--parseIf :: Parser Expr
parseIf = do
  reserved "if"
  e1 <- expr
  reserved "then"
  e2 <- expr
  reserved "else"
  If e1 e2 <$> expr

--parseCase :: Parser Expr
parseCase = do
  reserved "case"
  e <- expr
  reserved "of"
  symbol "{"
  alts <- parseAlts
  symbol "}"
  return (Case e alts)

--parseAlts :: Parser [(Pat, Expr)]
parseAlts = parseAlt `sepBy` reservedOp ";"

--parseAlt :: Parser (Pat, Expr)
parseAlt = do
  p <- parsePat
  reservedOp "->"
  e <- expr
  return (p, e)

--parsePat :: Parser Pat
parsePat = try parsePLit <|> try parsePVar <|> try parsePCon

--parsePVar :: Parser Pat
parsePVar = do
  PVar <$> identifier

--parsePLit :: Parser Pat
parsePLit = do
  PLit <$> parseLiteral

--parsePCon :: Parser Pat
parsePCon = do
  con <- identifier
  pats <- many parsePat
  return (PCon con pats)



lamAbs term = do char '\\'
                 i <- varId
                 char '.'
                 Lam i <$> term

parseNonApp =  try (parens expr) -- (E)
              <|> lamAbs expr                               -- \x.E
              <|> parseLit
              <|> parseIf
              <|> parseLet
              <|> parseCase
              <|> varOuCons
              <|> parseTuple

parseTuple = do
  symbol "("
  e1 <- expr
  symbol ","
  e2 <- expr
  symbol ")"
  return (App ( App (Cons "(,)") e1) e2)

varId = many1 letter

----------------------------------------
parseLambda s = case parseExpr s of
                     Left er -> print er
                     Right e -> print e >> print (infer e)

main = do putStr "Lambda:"
          e <- getLine
          parseLambda e