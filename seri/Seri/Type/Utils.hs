
{-# LANGUAGE PatternGuards #-}

module Seri.Type.Utils (
    assignments, isSubType, Assign(..), assign,
    nvarTs, varTs,
    ) where

import Control.Monad.State
import Data.List(nub)

import Seri.Name
import Seri.Type.Type

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
    
-- | Replace all variable types and numeric variable types with the given
-- values. Uses association list lookup.
assign :: (Assign a) => [(Name, Type)] -> a -> a
assign [] = id
assign m = assignl (\n -> Prelude.lookup n m)

class Assign a where
    assignl :: (Name -> Maybe Type) -> a -> a

instance Assign NType where
    assignl f t = 
      let me = assignl f
      in case t of
            AppNT o a b -> AppNT o (me a) (me b)
            VarNT n | Just (NumT t') <- f n -> t'
            _ -> t

instance Assign Type where
    assignl f t = 
      let me = assignl f
      in case t of
            AppT a b -> AppT (me a) (me b)
            VarT n | Just t' <- f n -> t'
            NumT nt -> NumT (assignl f nt)
            _ -> t

instance (Assign a) => Assign [a] where
    assignl f = map (assignl f)

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

