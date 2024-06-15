module Inferencer (infer, TypeError(..)) where

import Types

data TypeError = UnificationFail SimpleType SimpleType
               | InfiniteType Id SimpleType
               | UnboundVariable Id
               | OtherError String
               deriving (Show)

type Infer a = Either TypeError a

emptyEnv :: TypeEnv
emptyEnv = []

extendEnv :: (Id, SimpleType) -> TypeEnv -> TypeEnv
extendEnv (x, t) env = (x, t) : env

lookupEnv :: Id -> TypeEnv -> Infer SimpleType
lookupEnv x [] = Left $ UnboundVariable x
lookupEnv x ((y, t):env) = if x == y then return t else lookupEnv x env

-- Função de unificação simplificada
unify :: SimpleType -> SimpleType -> Infer SimpleType
unify (TVar a) t = return t
unify t (TVar a) = return t
unify (TCon c1) (TCon c2) | c1 == c2 = return $ TCon c1
unify (TArr t1 t2) (TArr t1' t2') = do
    t1'' <- unify t1 t1'
    t2'' <- unify t2 t2'
    return $ TArr t1'' t2''
unify t1 t2 = Left $ UnificationFail t1 t2

-- Inferência de tipos simplificada
infer :: TypeEnv -> Expr -> Infer SimpleType
infer env (Var x) = lookupEnv x env
infer env (Const x) = lookupEnv x env
infer env (Lam x e) = do
    tx <- lookupEnv x env
    te <- infer (extendEnv (x, tx) env) e
    return $ TArr tx te
infer env (App e1 e2) = do
    t1 <- infer env e1
    t2 <- infer env e2
    case t1 of
        TArr t11 t12 -> do
            t <- unify t2 t11
            return t12
        _ -> Left $ UnificationFail t1 t2
infer env (Lit (LitInt _)) = return $ TCon "Int"
infer env (Lit (LitBool _)) = return $ TCon "Bool"
infer env (If e1 e2 e3) = do
    t1 <- infer env e1
    t2 <- infer env e2
    t3 <- infer env e3
    unify t1 (TCon "Bool")
    unify t2 t3
infer env (Let (x, e1) e2) = do
    t1 <- infer env e1
    t2 <- infer (extendEnv (x, t1) env) e2
    return t2
infer env (Case e alts) = do
    te <- infer env e
    talts <- mapM (inferAlt env) alts
    let (ps, ts) = unzip talts
    mapM_ (unify te) ps
    return $ head ts

inferAlt :: TypeEnv -> (Pat, Expr) -> Infer (SimpleType, SimpleType)
inferAlt env (p, e) = do
    tp <- inferPat env p
    te <- infer env e
    return (tp, te)

inferPat :: TypeEnv -> Pat -> Infer SimpleType
inferPat env (PVar x) = lookupEnv x env
inferPat env (PLit (LitInt _)) = return $ TCon "Int"
inferPat env (PLit (LitBool _)) = return $ TCon "Bool"
inferPat env (PCon c ps) = do
    tc <- lookupEnv c env
    tps <- mapM (inferPat env) ps
    return $ foldl TApp tc tps
