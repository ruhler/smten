
-- Monomorhpic Target
--  Takes a polymorphic seri lambda expression, and compiles it to an
--  equivalent monomorphic seri lambda expression.
module Seri.Target.Monomorphic.Monomorphic (Monomorphic(..)) where

import Control.Monad.State
import Data.List((\\), nub)

import Seri.Failable
import Seri.Lambda

class Monomorphic a where
    monomorphic :: Env -> a -> (Env, a)

instance Monomorphic Exp where
    monomorphic env e = fst $ runState (monoalle e) (MS env [] [] [] [] [] [])

instance Monomorphic Type where
    monomorphic env t = fst $ runState (monoallt t) (MS env [] [] [] [] [] [])

data MS = MS {
    -- declarations in the original polymorphic environment.
    ms_poly :: [Dec],

    -- compiled declarations in the target monomorphic environment.
    ms_mono :: [Dec],

    -- Concrete types to make sure we monomorphize
    ms_totype :: [Type],

    -- Concretely typed variables to make sure we monomorphize
    ms_toexp :: [Sig],

    -- Concrete types we already monomorphized
    ms_typed :: [Type],

    -- Variables we already monomorphized
    ms_exped :: [Sig],

    -- List of locally bound variables.
    ms_bound :: [Name]
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
            eds <- mapM genval es
            modify $ \ms -> ms {
                ms_typed = dt ++ ts,
                ms_exped = de ++ es,
                ms_mono = (ms_mono ms) ++ (concat tds) ++ eds
             }
            finish

-- Generate a monomorphic declaration for the given concrete type.
gentype :: Type -> M [Dec]
gentype t = do
    poly <- gets ms_poly
    let (con, targs) = unfoldt t
    case attemptM $ lookupDataD poly con of
        Nothing -> return []
        (Just (DataD _ tvars cs)) -> do 
            let suffix = typesuffix t
            let mkc :: Con -> M Con
                mkc (Con n ts) = do
                    ts' <- mapM monotype ts
                    return (Con (n ++ suffix) ts')
            cs' <- mapM mkc (assign (zip tvars targs) cs)
            return [DataD (con ++ suffix) [] cs']

-- Generate a monomorphic declaration for the given concrete variable
genval :: Sig -> M Dec
genval s@(Sig n t) = do
    poly <- gets ms_poly
    t' <- monotype t
    case (attemptM $ lookupPrimD poly n) of
        Just d -> return d
        Nothing -> do
            suffix <- valsuffix s
            let n' = n ++ suffix
            (pt, e) <- attemptM $ lookupVar poly s
            e' <- monoexp $ assign (assignments pt t) e
            return $ ValD (TopSig n' [] t') e'

-- Translate a concretely typed expression to the appropriate monomorphic
-- expression.
monoexp :: Exp -> M Exp
monoexp e@(IntegerE {}) = return e
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
    bound <- gets ms_bound
    modify $ \ms -> ms { ms_bound = n : bound }
    e' <- monoexp e
    modify $ \ms -> ms { ms_bound = bound }
    return (LamE (Sig n t') e')
monoexp (ConE (Sig n t)) = do
    t' <- monotype t
    let n' = n ++ typesuffix (last $ unarrowsT t)
    return (ConE (Sig n' t'))
monoexp (VarE s@(Sig n t)) = do
    bound <- gets ms_bound
    poly <- gets ms_poly
    case (n `elem` bound, attemptM $ lookupVarInfo poly s) of
        (True, _) -> do
            t' <- monotype t
            return (VarE (Sig n t'))
        (_, Just Primitive) -> do
            modify $ \ms -> ms { ms_toexp = s : ms_toexp ms }
            t' <- monotype t
            return (VarE (Sig n t'))
        (_, Just Declared) -> do
            modify $ \ms -> ms { ms_toexp = s : ms_toexp ms }
            t' <- monotype t
            suffix <- valsuffix s
            return (VarE (Sig (n ++ suffix) t'))
        (_, Just (Instance (Class _ cts))) -> do
            modify (\ms -> ms { ms_toexp = s : ms_toexp ms })
            t' <- monotype t
            suffix <- valsuffix s
            return (VarE (Sig (n ++ suffix) t'))
        _ -> do
            t' <- monotype t
            return (VarE (Sig n t'))

monomatch :: Match -> M Match
monomatch (Match p e) = do
    p' <- monopat p
    bound <- gets ms_bound
    modify $ \ms -> ms { ms_bound = map fst (bindingsP p) ++ bound }
    e' <- monoexp e
    modify $ \ms -> ms { ms_bound = bound }
    return $ Match p' e'

monopat :: Pat -> M Pat
monopat (ConP t n ps) = do
    let n' = n ++ typesuffix t
    t' <- monotype t
    ps' <- mapM monopat ps
    return (ConP t' n' ps')
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
monoalle :: Exp -> M (Env, Exp)
monoalle e = do
    e' <- monoexp e
    finish
    m <- gets ms_mono
    return (m, e')

-- Monomorphize the given type and its environment.
monoallt :: Type -> M (Env, Type)
monoallt t = do
    t' <- monotype t
    finish
    m <- gets ms_mono
    return (m, t')

-- Give the monomorphic name for an applied type
mononametype :: Type -> Name
mononametype (ConT n) = n
mononametype (AppT a b) = mononametype a ++ "$" ++ mononametype b

mksuffix :: [Type] -> Name
mksuffix ts = foldl (\a b -> a ++ "$" ++ mononametype b) "" ts

-- Given a concrete fully applied type,
--  return the name suffix used for type and constructors of the type.
typesuffix :: Type -> Name
typesuffix = mksuffix . snd . unfoldt

valsuffix :: Sig -> M Name
valsuffix s@(Sig n t) = do
    poly <- gets ms_poly
    pt <- attemptM $ lookupVarType poly n
    case attemptM $ lookupVarInfo poly s of
        Just Declared ->
            return $ mksuffix (map snd (assignments pt t))
        Just (Instance (Class _ cts)) -> 
            return $ mksuffix (cts ++ map snd (assignments pt t))
    
-- Unfold a concrete type.
--  Foo a b ... c 
--   Is turned into ("Foo", [a, b, ... c])
unfoldt :: Type -> (Name, [Type])
unfoldt (ConT n) = (n, [])
unfoldt (AppT a b)
  = let (n, args) = unfoldt a
    in (n, args ++ [b])
unfoldt t = error $ "unfoldt: " ++ pretty t

