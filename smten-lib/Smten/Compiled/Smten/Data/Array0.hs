
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module Smten.Compiled.Smten.Data.Array0 (
    PrimArray, primArray, primSelect,
 ) where

import qualified Prelude as P

import Data.Array
import Data.List

import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula
import Smten.Compiled.Smten.Smten.Base

data PrimArray a = PrimArray (Array P.Int a)
                 | PrimArray_Ite BoolF (PrimArray a) (PrimArray a)

instance SmtenHS1 PrimArray where
    ite1 = PrimArray_Ite
    unreachable1 = P.error "PrimArray.unreachable"

instance SymbolicOf (Array P.Int a) (PrimArray a) where
    tosym = PrimArray

    symapp f x =
      case x of
        PrimArray x -> f x
        PrimArray_Ite p a b -> ite0 p (f $$ a) (f $$ b)

primArray :: (SmtenHS0 a) => List__ a -> PrimArray a
primArray = {-# SCC "PRIM_PRIMARRAY" #-} symapp (\xs -> PrimArray (listArray (0, genericLength xs) xs))

primSelect :: (SmtenHS0 a) => PrimArray a -> Int -> a
primSelect = {-# SCC "PRIM_PRIMSELECT" #-} symapp2 (\arr i -> arr ! (i :: P.Int))

