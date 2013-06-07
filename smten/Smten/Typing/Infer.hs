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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Typing.Infer (
        typeinfer
    ) where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map

import Smten.Failable
import Smten.Strict
import Smten.Location
import Smten.Name
import Smten.Sig
import Smten.Lit
import Smten.Type
import Smten.Exp
import Smten.Dec
import Smten.Module
import Smten.Typing.Solver

data TIS = TIS {
    ti_varid :: Integer,        -- ^ The next free VarT id
    ti_cons :: [(Type, Type)]   -- ^ A list of accumulated type constraints
}

data TIR = TIR {
    ti_tenv :: [Sig],           -- ^ Types of bound variables in scope
    ti_env :: Env,              -- ^ The environment
    ti_loc :: Location
}

type TI = ReaderT TIR (StateT TIS Failable)

withloc :: Location -> TI a -> TI a
withloc l = local (\ti -> ti { ti_loc = l })

instance (MonadErrorSL TI) where
    errloc = asks ti_loc

runTI :: Env -> TI a -> Failable a
runTI env x = evalStateT (runReaderT x (TIR [] env lunknown)) (TIS 1 [])

-- | Perform type inference on the given modules.
-- Types UnknownT are inferred.
--
-- The returned expression may have incorrectly inferred types if the
-- expression doesn't type check, so you should run typecheck after
-- inference to make sure it's valid.
typeinfer :: [Module] -> Failable [Module]
typeinfer ms = {-# SCC "TypeInfer" #-}
  runTI (environ ms) (mapM infermod ms)

-- Run inference on a single module.
infermod :: Module -> TI Module
infermod m = do
    ds <- mapM inferdec (mod_decs m)
    return $ m { mod_decs = ds }

-- Run inference on a single declaration
inferdec :: Dec -> TI Dec
inferdec (ValD l (TopExp ts@(TopSig n ctx t) e)) = withloc l $ do
    e' <- inferexp t e
    return $ ValD l (TopExp ts e')
inferdec d@(DataD {}) = return d
inferdec d@(ClassD l ctx n vars ms) = withloc l $ do
  let infermethod :: TopExp -> TI TopExp
      infermethod (TopExp ts@(TopSig _ _ t) e) = do
        e' <- inferexp t e
        return (TopExp ts e')
  ms' <- mapM infermethod ms
  return (ClassD l ctx n vars ms')
inferdec (InstD l ctx cls ms) = withloc l $ do
  let infermethod :: Method -> TI Method
      infermethod (Method n e) = do
         env <- asks ti_env
         t <- lookupMethodType env n cls
         e' <- inferexp t e
         return (Method n e')
  ms' <- mapM infermethod ms
  return (InstD l ctx cls ms')
inferdec d@(PrimD {}) = return d
inferdec d@(AsInHaskellD {}) = return d

inferexp :: Type -> Exp -> TI Exp
inferexp t e = do
 e' <- deunknown e
 modify $ \s -> s { ti_cons = [] }
 te' <- constrain e'
 addc t te'
 cons <- gets ti_cons
 let sol = solve cons
 return $ assignl (flip Map.lookup sol) e'

-- | Replace all UnknownT with new variable types.
-- State is the id of the next free type variable to use.
deunknown :: Exp -> TI Exp
deunknown = 
  let f UnknownT = do
          id <- gets ti_varid
          modifyS $ \s -> s { ti_varid = id+1 }
          return (VarT (name $ "~" ++ show id) UnknownK)
      f (AppT a b) = do
          a' <- f a
          b' <- f b
          return $ AppT a' b'
      f t = return t
  in transformMTE f


-- | Add a type constraint
addc :: Type -> Type -> TI ()
addc a b = modify $ \ti -> ti { ti_cons = (a, b) : (ti_cons ti) }

-- | Return a new variable type.
-- State is the id of the next free type variable to use.
newvt :: TI Type
newvt = do
    n <- newvtn
    return $ VarT n UnknownK

newvtn :: TI Name
newvtn = do
    id <- gets ti_varid
    modify $ \ti -> ti { ti_varid = (id + 1) }
    return . name $ ("~" ++ show id)

-- | Run type checking with additional bound variable's in scope.
scoped :: [Sig] -> TI a -> TI a
scoped vars = local (\ti -> ti { ti_tenv = vars ++ ti_tenv ti })

class Constrain a where
    -- | Generate type constraints for an expression, assuming no UnknownT types
    -- are in it.
    --
    -- Returns the type of the thing being constrained.
    constrain :: a -> TI Type

instance Constrain Lit where
    constrain l = return $ typeof l

instance Constrain Exp where
    constrain (LitE loc l) = withloc loc $ constrain l
    constrain (ConE l (Sig n t)) = withloc l $ do
        env <- asks ti_env
        cty <- lookupDataConType env n
        rcty <- retype cty
        addc rcty t
        return t
    constrain v@(VarE l (Sig n t)) = withloc l $ do
        tenv <- asks ti_tenv
        case lookup n (map (\(Sig n t) -> (n, t)) tenv) of
            Just t' -> addc t' t
            Nothing -> do
                env <- asks ti_env
                vt <- lookupVarType env n
                rvt <- retype vt
                addc rvt t
        return t
    constrain (AppE l f x) = withloc l $ do
        tf <- constrain f
        tx <- constrain x
        it <- newvt
        ot <- newvt
        addc (arrowT it ot) tf
        addc it tx
        return ot
    constrain (LamE l s x) = withloc l $ do
        ot <- scoped [s] (constrain x)
        return $ arrowT (typeof s) ot
    constrain (CaseE l x (Sig kn kt) y n) = withloc l $ do
        xt <- constrain x
        yt <- constrain y
        nt <- constrain n 

        env <- asks ti_env
        kty <- lookupDataConType env kn
        rkty <- retype kty
        addc rkty kt

        let at = last $ de_arrowsT rkty
        addc at xt

        let ywt = arrowsT (init (de_arrowsT rkty) ++ [nt])
        addc ywt yt
        return nt
    constrain (LetE l bs x) = withloc l $ scoped (map fst bs) $ do
        let f :: (Sig, Exp) -> TI ()
            f (Sig _ t, v) = do
                vt <- constrain v
                addc vt t
        mapM f bs
        constrain x

-- Given a type, return a new version of the type with new VarTs.
retype :: Type -> TI Type
retype t = do
    (t', _) <- runStateT (retype' t) []
    return t'
       where
    namefor :: Name -> StateT [(Name, Name)] TI Name
    namefor n = do
        names <- get
        case lookup n names of
            Nothing -> do
                n' <- lift newvtn
                put $ (n, n') : names
                return n'
            Just n' -> return n'
    
    retype' :: Type -> StateT [(Name, Name)] TI Type 
    retype' t@(ConT {}) = return t
    retype' t@(AppT a b) = do
        a' <- retype' a
        b' <- retype' b
        return $ AppT a' b'
    retype' (VarT n k) = do
        n' <- namefor n
        return (VarT n' k)
    retype' t@(NumT n) = return t
    retype' (OpT f a b) = do
        a' <- retype' a
        b' <- retype' b
        return $ OpT f a' b'
    retype' UnknownT = return UnknownT

