
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit0 (
    Bit, 
    bv_eq,
    bv_show,
    bv_fromInteger, bv_add, bv_sub,
    ) where

import Smten.Prelude
import Smten.Data.NumT
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

-- Primitive fixed-width bit-vector type.
data Bit n

bv_eq :: (Numeric n) => Bit n -> Bit n -> Bool
bv_eq = primitive "Smten.Data.Bit0.bv_eq"

bv_show :: (Numeric n) => Bit n -> String
bv_show = primitive "Smten.Data.Bit0.bv_show"

bv_fromInteger :: (Numeric n) => Integer -> Bit n
bv_fromInteger = primitive "Smten.Data.Bit0.bv_fromInteger"

bv_add :: (Numeric n) => Bit n -> Bit n -> Bit n
bv_add = primitive "Smten.Data.Bit0.bv_add"

bv_sub :: (Numeric n) => Bit n -> Bit n -> Bit n
bv_sub = primitive "Smten.Data.Bit0.bv_sub"


