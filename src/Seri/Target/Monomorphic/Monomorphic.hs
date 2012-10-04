-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

-- Monomorhpic Target
--  Takes a polymorphic seri lambda expression, and compiles it to an
--  equivalent monomorphic seri lambda expression.
module Seri.Target.Monomorphic.Monomorphic (Monomorphic(..),
  -- TODO: expose mononametype as a nicer interface, I put it here as a hack
  -- for use in the yices target.
  mononametype) where

import Control.Monad.State.Strict
import qualified Data.Set as Set

import Seri.Failable
import Seri.Lambda

class Monomorphic a where
    monomorphic :: Env -> a -> ([Dec], a)

instance Monomorphic Exp where
    monomorphic env e = fst $ runState (monoalle e) (MS env Set.empty Set.empty Set.empty Set.empty Set.empty [])

instance Monomorphic Type where
    monomorphic env t = fst $ runState (monoallt t) (MS env Set.empty Set.empty Set.empty Set.empty Set.empty [])

data MS = MS {
    -- declarations in the original polymorphic environment.
    ms_poly :: Env,

    -- compiled declarations in the target monomorphic environment.
    ms_mono :: Set.Set Dec,

    -- Concrete types to make sure we monomorphize
    ms_totype :: Set.Set Type,

    -- Concretely typed variables to make sure we monomorphize
    ms_toexp :: Set.Set Sig,

    -- Concrete types we already monomorphized
    ms_typed :: Set.Set Type,

    -- Variables we already monomorphized
    ms_exped :: Set.Set Sig,

    -- List of locally bound variables.
    ms_bound :: [Name]
}

type M = State MS

modifyS :: (MonadState s m) => (s -> s) -> m ()
modifyS f = do
    x <- get
    put $! f x

-- finish
--  Finish generating declarations for all the needed concrete types and VarEs
finish :: M ()
finish = do
    tt <- gets ms_totype
    dt <- gets ms_typed
    te <- gets ms_toexp
    de <- gets ms_exped
    case (tt Set.\\ dt, te Set.\\ de) of
        (a, b) | Set.null a && Set.null b -> return ()
        (ts, es) -> do
            modifyS $ \ms -> ms { ms_totype = Set.empty, ms_toexp = Set.empty }
            tds <- mapM gentype (Set.elems ts)
            eds <- mapM genval (Set.elems es)
            modifyS $ \ms -> ms {
                ms_typed = Set.union dt ts,
                ms_exped = Set.union de es,
                ms_mono = (ms_mono ms) `Set.union` (Set.fromList $ (concat tds) ++ eds)
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
                    return (Con (n `nappend` suffix) ts')
            cs' <- mapM mkc (assign (zip (map tyVarName tvars) targs) cs)
            return [DataD (con `nappend` suffix) [] cs']

-- Generate a monomorphic declaration for the given concrete variable
genval :: Sig -> M Dec
genval s@(Sig n t) = do
    poly <- gets ms_poly
    t' <- monotype t
    case (attemptM $ lookupPrimD poly n) of
        Just d -> return d
        Nothing -> do
            suffix <- valsuffix s
            let n' = n `nappend` suffix
            (pt, e) <- attemptM $ lookupVar poly s
            e' <- monoexp $ assign (assignments pt t) e
            return $ ValD (TopSig n' [] t') e'

-- Translate a concretely typed expression to the appropriate monomorphic
-- expression.
monoexp :: Exp -> M Exp
monoexp e@(LitE {}) = return e
monoexp (ConE (Sig n t)) = do
    t' <- monotype t
    let n' = n `nappend` typesuffix (last $ unarrowsT t)
    return (ConE (Sig n' t'))
monoexp (VarE s@(Sig n t)) = do
    bound <- gets ms_bound
    poly <- gets ms_poly
    t' <- monotype t
    case (n `elem` bound, attemptM $ lookupVarInfo poly s) of
        (True, _) -> return (VarE (Sig n t'))
        (_, Just Primitive) -> do
            modifyS $ \ms -> ms { ms_toexp = Set.insert s (ms_toexp ms) }
            return (VarE (Sig n t'))
        (_, Just Declared) -> do
            modifyS $ \ms -> ms { ms_toexp = Set.insert s (ms_toexp ms) }
            suffix <- valsuffix s
            return (VarE (Sig (n `nappend` suffix) t'))
        (_, Just (Instance (Class _ cts))) -> do
            modifyS (\ms -> ms { ms_toexp = Set.insert s (ms_toexp ms) })
            suffix <- valsuffix s
            return (VarE (Sig (n `nappend` suffix) t'))
        _ -> return (VarE (Sig n t'))
monoexp (AppE a bs) = do
    a' <- monoexp a
    bs' <- mapM monoexp bs
    return (AppE a' bs')
monoexp (LaceE ms) = do
    ms' <- mapM monomatch ms
    return (LaceE ms')

monomatch :: Match -> M Match
monomatch (Match ps e) = do
    ps' <- mapM monopat ps
    bound <- gets ms_bound
    modifyS $ \ms -> ms { ms_bound = concatMap bindingsP' ps ++ bound }
    e' <- monoexp e
    modifyS $ \ms -> ms { ms_bound = bound }
    return $ Match ps' e'

monopat :: Pat -> M Pat
monopat (ConP t n ps) = do
    let n' = n `nappend` typesuffix t
    t' <- monotype t
    ps' <- mapM monopat ps
    return (ConP t' n' ps')
monopat (VarP (Sig n t)) = do
    t' <- monotype t
    return (VarP (Sig n t'))
monopat p@(LitP {}) = return p
monopat (WildP t) = do
    t' <- monotype t
    return (WildP t')

-- Translate a concrete type to the appropriate monomorphic type.
monotype :: Type -> M Type
monotype (VarT {}) = error $ "variable type is not concrete"
monotype t = do
    poly <- gets ms_poly
    case unfoldt t of
        (n, targs) | n == name "->" -> do
            targsmono <- mapM monotype targs
            return $ foldl AppT (ConT (name "->")) targsmono
        (_, targs) -> do
            modifyS $ \ms -> ms { ms_totype = Set.insert t (ms_totype ms) }
            targsmono <- mapM monotype targs
            return $ ConT (mononametype t)

-- Monomorphize the given expression and its environment.
monoalle :: Exp -> M ([Dec], Exp)
monoalle e = do
    e' <- monoexp e
    finish
    m <- gets ms_mono
    return (Set.elems m, e')

-- Monomorphize the given type and its environment.
monoallt :: Type -> M ([Dec], Type)
monoallt t = do
    t' <- monotype t
    finish
    m <- gets ms_mono
    return (Set.elems m, t')

-- Give the monomorphic name for an applied type
mononametype :: Type -> Name
mononametype (ConT n) = n
mononametype (NumT n) = ntname n
mononametype (AppT a b) = mononametype a `nappend` name "$" `nappend` mononametype b

mksuffix :: [Type] -> Name
mksuffix ts = foldl (\a b -> a `nappend` name "$" `nappend` mononametype b) (name "") ts

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

ntname :: NType -> Name
ntname n = name "#" `nappend` name (show (nteval n))
    
-- Unfold a concrete type.
--  Foo a b ... c 
--   Is turned into ("Foo", [a, b, ... c])
unfoldt :: Type -> (Name, [Type])
unfoldt (ConT n) = (n, [])
unfoldt (NumT n) = (ntname n, [])
unfoldt (AppT a b)
  = let (n, args) = unfoldt a
    in (n, args ++ [b])
unfoldt t = error $ "unfoldt: " ++ pretty t

