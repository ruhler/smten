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

-- | Utilities for working with Seri Types
module Seri.Lambda.Types (
    appsT, arrowsT, outputT, unappsT, unarrowsT,
    listT, integerT, bitT, charT, stringT, tupT, untupT,
    Typeof(..), typeofCon,
    assign, assignl, assignments, bindingsP, bindingsP', varTs, nvarTs,
    isSubType,
    ) where

import Control.Monad.State
import Data.List(nub)
import Data.Maybe

import Seri.Lambda.IR
import Seri.Lambda.Generics

-- | The Integer type
integerT :: Type
integerT = ConT "Integer"

bitT :: Integer -> Type
bitT w = AppT (ConT "Bit") (NumT (ConNT w))

-- | The Char type
charT :: Type
charT = ConT "Char"

-- | The String type
stringT :: Type
stringT = listT charT

-- | Given the list of types [a, b, ..., c],
-- Return the applications of those types: (a b ... c)
-- The list must be non-empty.
appsT :: [Type] -> Type
appsT [] = error $ "appsT applied to empty list"
appsT ts = foldl1 AppT ts

-- | Given the list of types [a, b, ..., c]
-- Return the type (a -> b -> ... -> c)
-- The list must be non-empty.
arrowsT :: [Type] -> Type
arrowsT [] = error $ "arrowsT applied to empty list"
arrowsT [t] = t
arrowsT (t:ts) = appsT [ConT "->", t, arrowsT ts]

-- | Given a type of the form (a -> b), returns b.
-- TODO: this should throw an error if the given type is not a function type.
outputT :: Type -> Type
outputT (AppT (AppT (ConT "->") _) t) = t
outputT t = t

-- | Given a type of the form (a b ... c),
-- returns the list: [a, b, ..., c]
unappsT :: Type -> [Type]
unappsT (AppT a b) = (unappsT a) ++ [b]
unappsT t = [t]

-- | Given a type of the form (a -> b -> ... -> c),
--  returns the list: [a, b, ..., c]
unarrowsT :: Type -> [Type]
unarrowsT (AppT (AppT (ConT "->") a) b) = a : (unarrowsT b)
unarrowsT t = [t]


-- | Given a type a, returns the type [a].
listT :: Type -> Type
listT t = AppT (ConT "[]") t

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
assign m = assignl (\n -> Prelude.lookup n m)

class Typeof a where
    -- | Return the seri type of the given object, assuming the object is well
    -- typed. Behavior is undefined if the object is not well typed.
    --
    -- TODO: it would be nice if behavior was "UnknownT" if the object is not
    -- well typed.
    typeof :: a -> Type

instance Typeof Lit where
    typeof (IntegerL {}) = integerT
    typeof (CharL {}) = charT

instance Typeof Exp where
    typeof (LitE l) = typeof l
    typeof (CaseE _ (m:_)) = typeof m
    typeof (AppE f _) = outputT (typeof f)
    typeof (LamE tn e) = arrowsT [typeof tn, typeof e]
    typeof (ConE tn) = typeof tn
    typeof (VarE tn) = typeof tn
    typeof e = error $ "typeof: " ++ show e
    
instance Typeof Sig where
    typeof (Sig _ t) = t

instance Typeof Match where
    typeof (Match _ e) = typeof e

instance Typeof Pat where
    typeof (ConP t _ _) = t
    typeof (VarP tn) = typeof tn
    typeof (IntegerP _) = integerT
    typeof (WildP t) = t

-- | Get the type of a constructor in the given Data declaration.
-- Returns Nothing if there is no such constructor in the declaration.
typeofCon :: Dec -> Name -> Maybe Type
typeofCon (DataD dn vars cons) cn = do
    Con _ ts <- listToMaybe (filter (\(Con n _) -> n == cn) cons)
    return $ arrowsT (ts ++ [appsT (ConT dn : map tyVarType vars)])
typeofCon _ _ = Nothing


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
            modify $ \l -> ('#':n, NumT t) : l
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
bindingsP (IntegerP {}) = []
bindingsP (WildP {}) = []

-- | Extract the names of the variables bound by a pattern.
bindingsP' :: Pat -> [Name]
bindingsP' (ConP _ _ ps) = concatMap bindingsP' ps
bindingsP' (VarP (Sig n _)) = [n]
bindingsP' (IntegerP {}) = []
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

-- | (a, b, ...)
-- There must be at least one type given.
--
-- If exactly one type is given, that type is returned without tupling.
tupT :: [Type] -> Type
tupT [] = error $ "tupT on empty list"
tupT [x] = x
tupT es = foldl AppT (ConT $ "(" ++ replicate (length es - 1) ',' ++ ")") es

-- | Extract the types [a, b, c, ...] from a tuple type (a, b, c, ...)
-- If the type is not a tuple type, that single type is returned.
untupT :: Type -> [Type]
untupT t = 
   case unappsT t of
      (ConT ('(':',':_)):ts -> ts
      _ -> [t]

