
{-# LANGUAGE PatternGuards #-}

module Smten.Type.Utils (
    assignments, isSubType,
    Assign(..), assign,
    AssignK(..), assignk,
    VarTs(..), kindof,
    ) where

import Control.Monad.State
import Data.List(nub, genericLength)

import Smten.Name
import Smten.Type.Type
import Smten.Type.Sugar

-- | assignments poly concrete
-- Given a polymorphic type and a concrete type of the same form, return the
-- mapping from type variable name to concrete type.
assignments :: Type -> Type -> [(Name, Type)]
assignments (VarT n _) t = [(n, t)]
assignments (OpT _ a b) (OpT _ a' b') = assignments a a' ++ assignments b b'
assignments (AppT a b) (AppT a' b') = assignments a a' ++ assignments b b'
assignments _ _ = []

-- | isSubType t sub
-- Return True if 'sub' is a concrete type of 't'.
isSubType :: Type -> Type -> Bool
isSubType t sub
  = let isst :: Type -> Type -> State [(Name, Type)] Bool
        isst (ConT n _) (ConT n' _) = return (n == n')
        isst (NumT n) (NumT n') = return (n == n')
        isst (AppT a b) (AppT a' b') = do
            ar <- isst a a'
            br <- isst b b'
            return (ar && br)
        isst (OpT o a b) (OpT o' a' b') = do
            ar <- isst a a'
            br <- isst b b'
            return (o == o' && ar && br)
        isst (VarT n _) t = do
            modify $ \l -> (n, t) : l
            return True
        isst _ _ = return False

        (b, tyvars) = runState (isst t sub) []
        assignnub = nub tyvars
        namenub = nub (map fst assignnub)
     in length namenub == length assignnub && b
    
-- | Replace all variable types with the given
-- values. Uses association list lookup.
assign :: (Assign a) => [(Name, Type)] -> a -> a
assign [] = id
assign m = assignl (\n -> Prelude.lookup n m)

assignk :: (AssignK a) => [(Integer, Kind)] -> a -> a
assignk [] = id
assignk m = assignkl (\i -> Prelude.lookup i m)

class Assign a where
    assignl :: (Name -> Maybe Type) -> a -> a

instance Assign Type where
    assignl f t = 
      let me = assignl f
      in case t of
            AppT a b -> AppT (me a) (me b)
            VarT n _ | Just t' <- f n -> t'
            OpT o a b -> OpT o (me a) (me b)
            _ -> t

instance (Assign a) => Assign [a] where
    assignl f = map (assignl f)

class AssignK a where
    assignkl :: (Integer -> Maybe Kind) -> a -> a

instance AssignK Kind where
    assignkl f k =
      let me = assignkl f
      in case k of
            ArrowK a b -> ArrowK (me a) (me b)
            VarK i | Just k' <- f i -> k'
            _ -> k

instance AssignK Type where
    assignkl f t 
      | ConT n k <- t = ConT n (assignkl f k)
      | AppT a b <- t = AppT (assignkl f a) (assignkl f b)
      | VarT n k <- t = VarT n (assignkl f k)
      | otherwise = t

class VarTs a where
    -- | List the variable type names in a given object
    varTs :: a -> [(Name, Kind)]

instance VarTs Type where
    varTs (AppT a b) = nub $ varTs a ++ varTs b
    varTs (VarT n k) = [(n, k)]
    varTs (OpT o a b) = nub $ varTs a ++ varTs b
    varTs _ = []

instance (VarTs a) => VarTs [a] where
    varTs xs = nub $ concatMap varTs xs

kindof :: Type -> Kind
kindof t 
 | ConT {} <- t = StarK
 | AppT a _ <- t =
     case (kindof a) of
        ArrowK ka kb -> kb
        _ -> UnknownK
 | VarT _ k <- t = k
 | NumT {} <- t = NumK
 | OpT {} <- t = NumK
 | UnknownT <- t = UnknownK

