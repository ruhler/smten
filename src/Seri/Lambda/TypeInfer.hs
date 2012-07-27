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

module Seri.Lambda.TypeInfer (
        TypeInfer(..)
    ) where

import Debug.Trace

import Control.Monad.State
import Data.Generics

import Seri.Failable
import Seri.Lambda.Env
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types
import Seri.Lambda.TypeSolver

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
    typeinfer = inferdec
    
-- Run inference on a single declaration, given the environment.
inferdec :: Env -> Dec -> Failable Dec
inferdec env (ValD (TopSig n ctx t) e) = do
    e' <- inferexp env t e
    return $ ValD (TopSig n ctx t) e'
inferdec env d@(DataD {}) = return d
inferdec env d@(ClassD {}) = return d
inferdec env (InstD ctx cls ms) =
  let infermethod :: Method -> Failable Method
      infermethod (Method n e) = do
         t <- lookupMethodType env n cls
         e' <- inferexp env t e
         return (Method n e')
  in do
    ms' <- mapM infermethod ms
    return (InstD ctx cls ms')
inferdec _ d@(PrimD {}) = return d

inferexp :: Env -> Type -> Exp -> Failable Exp
inferexp env t e = do
 let (e', id) = runState (deunknown e) 1
 let ticomp = do
         te' <- constrain e'
         addc t te'
 (_, TIS _ cons _ _) <- runStateT ticomp (TIS id [] [] env)
 sol <- solve cons
 --trace ("e': " ++ pretty e') (return ())
 --trace ("constraints: " ++ pretty cons) (return ())
 --trace ("solution: " ++ pretty sol) (return ())
 return $ replace sol e'


-- | Replace all UnknownT with new variable types.
-- State is the id of the next free type variable to use.
deunknown :: (Data e) => e -> State Integer e
deunknown =
    let ununt UnknownT = do
            id <- get
            put (id+1)
            return (VarT $ "~" ++ show id)
        ununt t = return t
    in everywhereM (mkM ununt) 


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
    return $ VarT n

newvtn :: TI Name
newvtn = do
    id <- gets ti_varid
    modify $ \ti -> ti { ti_varid = (id + 1) }
    return ("~" ++ show id)

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
    constrain (IntegerL {}) = return integerT
    constrain (CharL {}) = return charT

instance Constrain Exp where
    constrain (LitE l) = constrain l
    constrain (CaseE e ms) = do
        te <- constrain e
        tps <- mapM constrain [p | Match p _ <- ms]
        tms <- mapM constrain ms
        sequence_ [addc te tp | tp <- tps]
        sequence_ [addc (head tms) tm | tm <- tail tms]
        return (head tms)
    constrain (AppE f x) = do
        tf <- constrain f
        tx <- constrain x
        it <- newvt
        ot <- newvt
        addc (arrowsT [it, ot]) tf
        addc it tx
        return ot
    constrain (LamE (Sig n t) b) = do
        bt <- scoped [Sig n t] (constrain b)
        return (arrowsT [t, bt])
    constrain (ConE (Sig n t)) = do
        env <- gets ti_env
        cty <- lift $ lookupDataConType env n
        rcty <- retype cty
        addc rcty t
        return t
    constrain v@(VarE (Sig n t)) = do
        tenv <- gets ti_tenv
        case lookup n (map (\(Sig n t) -> (n, t)) tenv) of
            Just t' -> addc t' t
            Nothing -> do
                env <- gets ti_env
                vt <- lift $ lookupVarType env n
                rvt <- retype vt
                addc rvt t
        return t

-- Only constrains the body. Doesn't constrain the patterns.
instance Constrain Match where
    constrain (Match p e) = scoped (bindingsP p) (constrain e)

instance Constrain Pat where
    constrain (ConP t n ps) = do
        tps <- mapM constrain ps
        env <- gets ti_env
        cty <- lift $ lookupDataConType env n
        rcty <- retype cty
        addc rcty (arrowsT (tps ++ [t]))
        let pts = init (unarrowsT rcty)
        sequence_ [addc pt tp | (pt, tp) <- zip pts tps]
        return t
    constrain (VarP (Sig _ t)) = return t
    constrain (IntegerP {}) = return integerT
    constrain (WildP t) = return t


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
    retype' (VarT n) = do
        n' <- namefor n
        return (VarT n')
    retype' (NumT n) = do
        n' <- retypen n
        return (NumT n')
    retype' UnknownT = return UnknownT

    retypen :: NType -> StateT [(Name, Name)] TI NType
    retypen t@(ConNT {}) = return t
    retypen (VarNT n) = do
        n' <- namefor n
        return (VarNT n')
    retypen (AppNT o a b) = do
        a' <- retypen a
        b' <- retypen b
        return (AppNT o a' b')

-- If the given type is in the map, replace it, otherwise keep it unchanged.
replace :: (Data a) => [(Type, Type)] -> a -> a
replace m =
    let base :: Type -> Type
        base t =
            case lookup t m of
                Just t' -> t'
                Nothing -> t
    in everywhere $ mkT base
            
