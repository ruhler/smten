
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit0 (
    Bit, 
    bv_eq, bv_leq,
    bv_show,
    bv_fromInteger, bv_add, bv_sub, bv_mul,
    bv_and, bv_or, bv_shl, bv_lshr, bv_not,
    bv_concat, bv_extract, bv_sign_extend,
    bv_width, bv_value,
    ) where

import GHC.TypeLits
import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

-- Primitive fixed-width bit-vector type.
data Bit (n :: Nat)

bv_eq :: Bit n -> Bit n -> Bool
bv_eq = primitive "Smten.Data.Bit0.bv_eq"

bv_leq :: Bit n -> Bit n -> Bool
bv_leq = primitive "Smten.Data.Bit0.bv_leq"

bv_show :: Bit n -> String
bv_show = primitive "Smten.Data.Bit0.bv_show"

bv_fromInteger :: (SingI n) => Integer -> Bit n
bv_fromInteger = primitive "Smten.Data.Bit0.bv_fromInteger"

bv_add :: Bit n -> Bit n -> Bit n
bv_add = primitive "Smten.Data.Bit0.bv_add"

bv_sub :: Bit n -> Bit n -> Bit n
bv_sub = primitive "Smten.Data.Bit0.bv_sub"

bv_mul :: Bit n -> Bit n -> Bit n
bv_mul = primitive "Smten.Data.Bit0.bv_mul"

bv_and :: Bit n -> Bit n -> Bit n
bv_and = primitive "Smten.Data.Bit0.bv_and"

bv_or :: Bit n -> Bit n -> Bit n
bv_or = primitive "Smten.Data.Bit0.bv_or"

bv_shl :: (SingI n) => Bit n -> Bit n -> Bit n
bv_shl = primitive "Smten.Data.Bit0.bv_shl"

bv_lshr :: (SingI n) => Bit n -> Bit n -> Bit n
bv_lshr = primitive "Smten.Data.Bit0.bv_lshr"

bv_not :: Bit n -> Bit n
bv_not = primitive "Smten.Data.Bit0.bv_not"

bv_concat :: Bit a -> Bit b -> Bit (a+b)
bv_concat = primitive "Smten.Data.Bit0.bv_concat"

bv_extract :: (SingI n) => Bit m -> Integer -> Bit n
bv_extract = primitive "Smten.Data.Bit0.bv_extract"

bv_sign_extend :: (SingI m, SingI n) => Bit m -> Bit n
bv_sign_extend = primitive "Smten.Data.Bit0.bv_sign_extend"

bv_width :: (SingI n) => Bit n -> Integer
bv_width = primitive "Smten.Data.Bit0.bv_width"

bv_value :: Bit n -> Integer
bv_value = primitive "Smten.Data.Bit0.bv_value"

