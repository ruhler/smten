
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.Array (
    PrimArray, primArray, primSelect,
    P.Integer,
    ) where

import Prelude as P

import Data.Array
import Data.Functor
import Data.List(genericLength)

import Smten.Runtime.SmtenHS as S
import Smten.SMT.FreeID

data PrimArray a = PrimArray (Array P.Integer a)
                 | PrimArray_Prim (Assignment -> PrimArray a) (Cases (PrimArray a))

instance SmtenHS1 PrimArray where
    realize1 m (PrimArray x) = PrimArray (realize0 m <$> x)
    realize1 m (PrimArray_Prim r _) = realize0 m (r m)

    cases1 x@(PrimArray {}) = concrete x
    cases1 (PrimArray_Prim _ c) = c

    primitive1 = PrimArray_Prim

instance Haskelly (PrimArray a) (PrimArray a) where
    stohs = id
    frhs = id

instance (Haskelly h s) => Haskelly (PrimArray h) (PrimArray s) where
    stohs (PrimArray arr) = PrimArray (stohs <$> arr)
    frhs (PrimArray arr) = PrimArray (frhs <$> arr)

primArray :: [a] -> PrimArray a
primArray xs = PrimArray (listArray (0, genericLength xs) xs)

primSelect :: PrimArray a -> P.Integer -> a
primSelect (PrimArray arr) i = arr ! i

