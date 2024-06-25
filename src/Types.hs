module Types where

data Literal = LitInt Integer | LitBool Bool deriving (Show, Eq)

type Id = String

data Expr = Var Id
          | Const Id
          | App Expr Expr
          | Lam Id Expr
          | Lit Literal
          | If Expr Expr Expr
          | Case Expr [(Pat, Expr)]
          | Let (Id, Expr) Expr
          | Tuple Expr Expr
          | Cons Id
          deriving (Eq, Show)

data Pat = PVar Id
         | PLit Literal
         | PCon Id [Pat]
         deriving (Eq, Show)

data SimpleType = TVar Id
                | TArr SimpleType SimpleType
                | TCon String
                | TApp SimpleType SimpleType
                | TGen Int
                deriving (Eq, Show)

type Context = [(Id, SimpleType)]
type TypeEnv = Context

iniCont :: Context
iniCont = [("(,)", TArr (TGen 0) (TArr (TGen 1) (TApp (TApp (TCon "(,)") (TGen 0)) (TGen 1)))),
           ("True", TCon "Bool"),
           ("False", TCon "Bool")]