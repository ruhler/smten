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
        TypeInfer(..)
    ) where

import Debug.Trace

import Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe(fromMaybe)

import Smten.Failable
import Smten.Name
import Smten.Sig
import Smten.Lit
import Smten.Type
import Smten.Exp
import Smten.Dec
import Smten.Ppr(pretty)
import Smten.Typing.Solver


class TypeInfer a where
    -- | Perform type inference on the given object.
    -- Types UnknownT are inferred.
    --
    -- The returned expression may have incorrectly inferred types if the
    -- expression doesn't type check, so you should run typecheck after
    -- inference to make sure it's valid.
    typeinfer :: Env -> a -> Failable a

instance TypeInfer [Dec] where
    typeinfer e = mapM (typeinfer e)

instance TypeInfer Dec where
    typeinfer e d = onfail (\msg -> throw $ msg ++ "\nWhen running type inference on " ++ pretty d) $
       inferdec e d

-- Run inference on a single declaration, given the environment.
inferdec :: Env -> Dec -> Failable Dec
inferdec env (ValD l (TopExp ts@(TopSig n ctx t) e)) = do
    e' <- inferexp env t e
    return $ ValD l (TopExp ts e')
inferdec env d@(DataD {}) = return d
inferdec env d@(ClassD l ctx n vars ms) = do
  let infermethod :: TopExp -> Failable TopExp
      infermethod (TopExp ts@(TopSig _ _ t) e) = do
        e' <- inferexp env t e
        return (TopExp ts e')
  ms' <- mapM infermethod ms
  return (ClassD l ctx n vars ms')
inferdec env (InstD l ctx cls ms) = do
  let infermethod :: Method -> Failable Method
      infermethod (Method n e) = do
         t <- lookupMethodType env n cls
         e' <- inferexp env t e
         return (Method n e')
  ms' <- mapM infermethod ms
  return (InstD l ctx cls ms')
inferdec _ d@(PrimD {}) = return d

inferexp :: Env -> Type -> Exp -> Failable Exp
inferexp env t e = do
 let (e', id) = runState (deunknown e) 1
     ticomp = do
         te' <- constrain e'
         addc t te'
 (_, TIS _ cons _ _) <- runStateT ticomp (TIS id [] [] env)
 let sol = solve cons
 --trace ("e': " ++ pretty e') (return ())
 --trace ("constraints: " ++ pretty cons) (return ())
 --trace ("solution: " ++ pretty sol) (return ())
 return $ assignl (flip Map.lookup sol) e'

-- | Replace all UnknownT with new variable types.
-- State is the id of the next free type variable to use.
deunknown :: Exp -> State Integer Exp
deunknown = 
  let f UnknownT = do
          id <- get
          put (id+1)
          return (VarT (name $ "~" ++ show id) UnknownK)
      f (AppT a b) = do
          a' <- f a
          b' <- f b
          return $ AppT a' b'
      f t = return t
  in transformMTE f

data TIS = TIS {
    ti_varid :: Integer,        -- ^ The next free VarT id
    ti_cons :: [(Type, Type)],  -- ^ A list of accumulated type constraints
    ti_tenv :: [Sig],           -- ^ Types of bound variables in scope
    ti_env :: Env               -- ^ The environment
}

type TI = StateT TIS Failable

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
scoped vars x = do
    tenv <- gets ti_tenv
    modify $ \ti -> ti { ti_tenv = vars ++ tenv }
    r <- x
    modify $ \ti -> ti { ti_tenv = tenv }
    return r

class Constrain a where
    -- | Generate type constraints for an expression, assuming no UnknownT types
    -- are in it.
    --
    -- Returns the type of the thing being constrained.
    constrain :: a -> TI Type

instance Constrain Lit where
    constrain l = return $ typeof l

instance Constrain Exp where
    constrain (LitE _ l) = constrain l
    constrain (ConE _ (Sig n t)) = do
        env <- gets ti_env
        cty <- lift $ lookupDataConType env n
        rcty <- retype cty
        addc rcty t
        return t
    constrain v@(VarE _ (Sig n t)) = do
        tenv <- gets ti_tenv
        case lookup n (map (\(Sig n t) -> (n, t)) tenv) of
            Just t' -> addc t' t
            Nothing -> do
                env <- gets ti_env
                vt <- lift $ lookupVarType env n
                rvt <- retype vt
                addc rvt t
        return t
    constrain (AppE _ f x) = do
        tf <- constrain f
        tx <- constrain x
        it <- newvt
        ot <- newvt
        addc (arrowT it ot) tf
        addc it tx
        return ot
    constrain (LamE _ s x) = do
        ot <- scoped [s] (constrain x)
        return $ arrowT (typeof s) ot
    constrain (CaseE _ x (Sig kn kt) y n) = do
        xt <- constrain x
        yt <- constrain y
        nt <- constrain n 

        env <- gets ti_env
        kty <- lift $ lookupDataConType env kn
        rkty <- retype kty
        addc rkty kt

        let at = last $ de_arrowsT rkty
        addc at xt

        let ywt = arrowsT (init (de_arrowsT rkty) ++ [nt])
        addc ywt yt
        return nt

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

