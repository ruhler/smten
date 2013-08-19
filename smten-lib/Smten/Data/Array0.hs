
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Array0 (
    PrimArray, primArray, primSelect,
    ) where

import Prelude
import Data.Array

import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

data PrimArray a = PrimArray (Array Int a)

{-# NOINLINE primArray #-}
primArray :: [a] -> PrimArray a
primArray xs = PrimArray (listArray (0, length xs) xs)

{-# NOINLINE primSelect #-}
primSelect :: PrimArray a -> Int -> a
primSelect (PrimArray x) i = x ! i

