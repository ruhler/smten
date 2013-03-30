
{-# LANGUAGE PatternGuards #-}

module Smten.Type.Utils (
    Assign(..), assign,
    AssignK(..), assignk,
    VarTs(..), kindof,
    ) where

import Debug.Trace

import Control.Monad.State
import Data.List(nub)

import Smten.Name
import Smten.Type.Type
import Smten.Type.Sugar
import Smten.Type.Canonical

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

