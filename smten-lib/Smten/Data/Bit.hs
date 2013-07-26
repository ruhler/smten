
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit (
    Bit,
    bv_and, bv_or, bv_xor, bv_shl, bv_lshr, bv_not,
    ) where

import Smten.Prelude
import Smten.Smten.TypeLits
import Smten.Data.Bit0

instance Eq (Bit n) where
    (==) = bv_eq

instance Ord (Bit n) where
    (<=) = bv_leq

instance Show (Bit n) where
    show = bv_show

instance (SingI n) => Num (Bit n) where
    (+) = bv_add
    (-) = bv_sub
    (*) = bv_mul
    abs = error "TODO: Bit.abs"
    signum = error "TODO: Bit.signum"
    fromInteger = bv_fromInteger

bv_xor :: Bit n -> Bit n -> Bit n
bv_xor a b = bv_and (bv_or a b) (bv_not (bv_and a b))

