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

{-# LANGUAGE PatternGuards #-}

-- | Utilities for working with Seri Types
module Seri.Lambda.Types (
    assign, assignl, assignments, bindingsP, bindingsP', varTs, nvarTs,
    isSubType,
    ) where

import Control.Monad.State
import Data.Functor
import Data.List(nub)
import Data.Maybe

import Seri.Lambda.IR
import Seri.Lambda.Generics
import Seri.Type

-- | assignments poly concrete
-- Given a polymorphic type and a concrete type of the same form, return the
-- mapping from type variable name to concrete type.
assignments :: Type -> Type -> [(Name, Type)]
assignments (VarT n) t = [(n, t)]
assignments (NumT (VarNT n)) t = [(n, t)]
assignments (NumT (AppNT _ a b)) (NumT (AppNT _ a' b'))
    = assignments (NumT a) (NumT a') ++ assignments (NumT b) (NumT b')
assignments (AppT a b) (AppT a' b') = (assignments a a') ++ (assignments b b')
assignments _ _ = []


newtype Assign = Assign (Name -> Maybe Type)

instance Transformer Assign where
    t_Type (Assign lookup) t@(VarT n) =
      case lookup n of
        Just t' -> t'
        Nothing -> t
    t_Type _ t = t

    t_NType (Assign lookup) t@(VarNT n) =
      case lookup n of
        Just (NumT t') -> t'
        Nothing -> t
    t_NType _ t = t

-- | Replace all variable types and numeric variable types with the given
-- values. Takes a generic lookup function.
assignl :: Transformable a => (Name -> Maybe Type) -> a -> a
assignl lookup = transform (Assign lookup)

-- | Replace all variable types and numeric variable types with the given
-- values. Uses association list lookup.
assign :: Transformable a => [(Name, Type)] -> a -> a
assign [] = id
assign m = assignl (\n -> Prelude.lookup n m)

instance Typeof Exp where
    typeof (LitE l) = typeof l
    typeof (ConE tn) = typeof tn
    typeof (VarE tn) = typeof tn
    typeof e@(AppE f x) = fromMaybe UnknownT $ snd <$> de_arrowT (typeof f)
    typeof (LaceE []) = UnknownT
    typeof (LaceE (Match ps b:_)) = arrowsT (map typeof ps ++ [typeof b])
    
instance Typeof Match where
    typeof (Match _ e) = typeof e

instance Typeof Pat where
    typeof (ConP t _ _) = t
    typeof (VarP tn) = typeof tn
    typeof (LitP l) = typeof l
    typeof (WildP t) = t

-- | isSubType t sub
-- Return True if 'sub' is a concrete type of 't'.
isSubType :: Type -> Type -> Bool
isSubType t sub
  = let isst :: Type -> Type -> State [(Name, Type)] Bool
        isst (ConT n) (ConT n') = return (n == n')
        isst (AppT a b) (AppT a' b') = do
            ar <- isst a a'
            br <- isst b b'
            return (ar && br)
        isst (VarT n) t = do
            modify $ \l -> (n, t) : l
            return True
        isst (NumT a) (NumT b) = isstn a b
        isst _ _ = return False

        isstn :: NType -> NType -> State [(Name, Type)] Bool
        isstn (ConNT n) (ConNT n') = return (n == n')
        isstn (AppNT o a b) (AppNT o' a' b') = do
            ar <- isstn a a'
            br <- isstn b b'
            return (o == o' && ar && br)
        isstn (VarNT n) t = do
            modify $ \l -> (ncons '#' n, NumT t) : l
            return True
        isstn _ _ = return False

        (b, tyvars) = runState (isst t sub) []
        assignnub = nub tyvars
        namenub = nub (map fst assignnub)
     in length namenub == length assignnub && b
    

-- | Extract the types of the variables bound by a pattern.
bindingsP :: Pat -> [Sig]
bindingsP (ConP _ _ ps) = concatMap bindingsP ps
bindingsP (VarP s) = [s]
bindingsP (LitP {}) = []
bindingsP (WildP {}) = []

-- | Extract the names of the variables bound by a pattern.
bindingsP' :: Pat -> [Name]
bindingsP' (ConP _ _ ps) = concatMap bindingsP' ps
bindingsP' (VarP (Sig n _)) = [n]
bindingsP' (LitP {}) = []
bindingsP' (WildP {}) = []

-- | List the (non-numeric) variable type names in a given type.
varTs :: Type -> [Name]
varTs (AppT a b) = nub $ varTs a ++ varTs b
varTs (VarT n) = [n]
varTs _ = []

-- | List the numeric type variables in the given type.
nvarTs :: Type -> [Name]
nvarTs (AppT a b) = nub $ nvarTs a ++ nvarTs b
nvarTs (NumT (AppNT _ a b)) = nub $ nvarTs (NumT a) ++ nvarTs (NumT b)
nvarTs (NumT (VarNT n)) = [n]
nvarTs _ = []

