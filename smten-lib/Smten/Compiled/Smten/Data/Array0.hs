
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
import Smten.Runtime.Types
import Smten.Compiled.Smten.Smten.Base

data PrimArray a = PrimArray (Array P.Int a)
                 | PrimArray_Ite Bool (PrimArray a) (PrimArray a)
                 | PrimArray_Err ErrorString

instance SmtenHS1 PrimArray where
    error1 = PrimArray_Err
    realize1 m x = 
      case x of
        PrimArray {} -> x
        PrimArray_Ite p a b -> iterealize p a b m
        PrimArray_Err msg -> PrimArray_Err (realize m msg)
    ite1 = PrimArray_Ite

instance SymbolicOf (Array P.Int a) (PrimArray a) where
    tosym = PrimArray

    symapp f x =
      case x of
        PrimArray x -> f x
        PrimArray_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        PrimArray_Err msg -> error0 msg

primArray :: (SmtenHS0 a) => List__ a -> PrimArray a
primArray = {-# SCC "PRIM_PRIMARRAY" #-} symapp (\xs -> PrimArray (listArray (0, genericLength xs) xs))

primSelect :: (SmtenHS0 a) => PrimArray a -> Int -> a
primSelect = {-# SCC "PRIM_PRIMSELECT" #-} symapp2 (\arr i -> arr ! (i :: P.Int))

