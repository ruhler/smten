
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

data PrimArray a = PrimArray (Array P.Integer a)
                 | PrimArrayMux S.Bool (PrimArray a) (PrimArray a)

instance SmtenHS1 PrimArray where
    mux1 = PrimArrayMux
    realize1 m x@(PrimArray {}) = x
    realize1 m (PrimArrayMux p a b) = S.__caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
    strict_app1 f (PrimArrayMux p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
    strict_app1 f c = f c

instance (Haskelly h s) => Haskelly (PrimArray h) (PrimArray s) where
    tohs (PrimArray arr) = PrimArray (tohs <$> arr)
    frhs (PrimArray arr) = PrimArray (frhs <$> arr)

primArray :: [a] -> PrimArray a
primArray xs = PrimArray (listArray (0, genericLength xs) xs)

primSelect :: PrimArray a -> P.Integer -> a
primSelect (PrimArray arr) i = arr ! i

