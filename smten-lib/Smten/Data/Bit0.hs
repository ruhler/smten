
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit0 (
    Bit, 
    bv_eq, bv_leq,
    bv_show,
    bv_fromInteger, bv_add, bv_sub,
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

