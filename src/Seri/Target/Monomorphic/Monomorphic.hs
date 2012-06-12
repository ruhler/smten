
-- Monomorhpic Target
--  Takes a polymorphic seri lambda expression, and compiles it to an
--  equivalent monomorphic seri lambda expression.
module Seri.Target.Monomorphic.Monomorphic (monomorphic) where

import Control.Monad.State
import Data.List((\\), nub)

import Seri.Lambda
import Seri.Utils.Ppr

monomorphic :: Env Exp -> Env Exp
monomorphic e =
  fst $ runState (monoall $ val e) (MS (decls e) [] [] [] [] [])

data MS = MS {
    -- declarations in the original polymorphic environment.
    ms_poly :: [Dec],

    -- compiled declarations in the target monomorphic environment.
    ms_mono :: [Dec],

    -- Concrete types to make sure we monomorphize
    ms_totype :: [Type],

    -- Concretely typed VarEs to make sure we monomorphize
    ms_toexp :: [Exp],

    -- Concrete types we already monomorphized
    ms_typed :: [Type],

    -- VarEs we already monomorphized
    ms_exped :: [Exp]
}

type M = State MS

-- finish
--  Finish generating declarations for all the needed concrete types and VarEs
finish :: M ()
finish = do
    tt <- gets ms_totype
    dt <- gets ms_typed
    te <- gets ms_toexp
    de <- gets ms_exped
    case (nub tt \\ dt, nub te \\ de) of
        ([], []) -> return ()
        (ts, es) -> do
            modify $ \ms -> ms { ms_totype = [], ms_toexp = [] }
            tds <- mapM gentype ts
            eds <- mapM genexp es
            modify $ \ms -> ms {
                ms_typed = dt ++ ts,
                ms_exped = de ++ es,
                ms_mono = (ms_mono ms) ++ tds ++ eds
             }
            finish

-- Generate a monomorphic declaration for the given concrete type.
gentype :: Type -> M Dec
gentype t = do
    poly <- gets ms_poly
    let (con, targs) = unfoldt t
    case lookupDataD (mkenv poly ()) con of
        Nothing -> error $ "gentype: " ++ con ++ " not found for " ++ render (ppr t)
        (Just (DataD _ tvars cs)) -> do 
            let suffix = typesuffix t
            let mkc :: Con -> M Con
                mkc (Con n ts) = do
                    ts' <- mapM monotype ts
                    return (Con (n ++ suffix) ts')
            cs' <- mapM mkc (assign (zip tvars targs) cs)
            return (DataD (con ++ suffix) [] cs')

-- Generate a monomorphic declaration for the given concrete VarE.
genexp :: Exp -> M Dec
genexp v@(VarE (Sig n t) _) = do
    t' <- monotype t
    suffix <- expsuffix v
    let n' = n ++ suffix
    poly <- gets ms_poly
    let Just (pt, e) = lookupvar (mkenv poly v)
    e' <- monoexp $ assign (assignments pt t) e
    return $ ValD (Sig n' t') e'

-- Translate a concretely typed expression to the appropriate monomorphic
-- expression.
monoexp :: Exp -> M Exp
monoexp e@(IntegerE {}) = return e
monoexp (PrimE (Sig n t)) = do
    t' <- monotype t
    return (PrimE (Sig n t'))
monoexp (CaseE e ms) = do
    e' <- monoexp e
    ms' <- mapM monomatch ms
    return (CaseE e' ms')
monoexp (AppE a b) = do
    a' <- monoexp a
    b' <- monoexp b
    return (AppE a' b')
monoexp (LamE (Sig n t) e) = do
    t' <- monotype t
    e' <- monoexp e
    return (LamE (Sig n t') e')
monoexp (ConE (Sig n t)) = do
    t' <- monotype t
    let n' = n ++ typesuffix (outputT t)
    return (ConE (Sig n' t'))
monoexp (VarE (Sig n t) Bound) = do
    t' <- monotype t
    return (VarE (Sig n t') Bound)
monoexp e@(VarE (Sig n t) Declared) = do
    modify $ \ms -> ms { ms_toexp = e : ms_toexp ms }
    t' <- monotype t
    suffix <- expsuffix e
    return (VarE (Sig (n ++ suffix) t') Declared)
monoexp e@(VarE (Sig n t) (Instance (Class _ cts))) = do
    modify $ \ms -> ms { ms_toexp = e : ms_toexp ms }
    t' <- monotype t
    suffix <- expsuffix e
    return (VarE (Sig (n ++ suffix) t') Declared)

monomatch :: Match -> M Match
monomatch (Match p e) = do
    p' <- monopat p
    e' <- monoexp e
    return $ Match p' e'

monopat :: Pat -> M Pat
monopat (ConP (Sig n t) ps) = do
    let n' = n ++ typesuffix (outputT t)
    t' <- monotype t
    ps' <- mapM monopat ps
    return (ConP (Sig n' t') ps')
monopat (VarP (Sig n t)) = do
    t' <- monotype t
    return (VarP (Sig n t'))
monopat p@(IntegerP {}) = return p
monopat (WildP t) = do
    t' <- monotype t
    return (WildP t')

-- Translate a concrete type to the appropriate monomorphic type.
monotype :: Type -> M Type
monotype (VarT {}) = error $ "variable type is not concrete"
monotype (ForallT {}) = error $ "forall type is not concrete"
monotype t@(ConT "Integer") = return t
monotype t@(ConT "Bool") = return t
monotype t@(ConT "()") = return t
monotype t = do
    poly <- gets ms_poly
    case unfoldt t of
        ("->", targs) -> do
            targsmono <- mapM monotype targs
            return $ foldl AppT (ConT "->") targsmono
        (_, targs) -> do
            modify $ \ms -> ms { ms_totype = t : ms_totype ms }
            targsmono <- mapM monotype targs
            return $ ConT (mononametype t)

-- Monomorphize the given expression and its environment.
monoall :: Exp -> M (Env Exp)
monoall e = do
    e' <- monoexp e
    finish
    m <- gets ms_mono
    return (mkenv m e')

-- Give the monomorphic name for an applied type
mononametype :: Type -> Name
mononametype (ConT n) = n
mononametype (AppT a b) = mononametype a ++ "$" ++ mononametype b

mksuffix :: [Type] -> Name
mksuffix [] = ""
mksuffix [t] = "$" ++ mononametype t
mksuffix (t:ts) = foldl (\a b -> a ++ "$" ++ mononametype b) "" ts

-- Given a concrete fully applied type,
--  return the name suffix used for type and constructors of the type.
typesuffix :: Type -> Name
typesuffix = mksuffix . snd . unfoldt

expsuffix :: Exp -> M Name
expsuffix e@(VarE (Sig n t) Declared) = do
    poly <- gets ms_poly
    let Just (pt, _) = lookupvar (mkenv poly e)
    return $ mksuffix (map snd (assignments pt t))
expsuffix e@(VarE (Sig n t) (Instance (Class _ cts))) = do
    poly <- gets ms_poly
    let Just (pt, _) = lookupvar (mkenv poly e)
    return $ mksuffix (cts ++ map snd (assignments pt t))
    
-- Unfold a concrete type.
--  Foo a b ... c 
--   Is turned into ("Foo", [a, b, ... c])
unfoldt :: Type -> (Name, [Type])
unfoldt (ConT n) = (n, [])
unfoldt (AppT a b)
  = let (n, args) = unfoldt a
    in (n, args ++ [b])

