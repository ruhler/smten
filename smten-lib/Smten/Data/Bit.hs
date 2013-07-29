
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit (
    Bit,
    bv_and, bv_or, bv_xor, bv_shl, bv_lshr, bv_not, bv_concat,
    bv_extract, bv_truncate, bv_zero_extend, bv_sign_extend,
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

bv_zero_extend :: forall n (m :: Nat) m_plus_n . (SingI m) => Bit n -> Bit m_plus_n
bv_zero_extend = bv_concat (0 :: Bit m)

bv_truncate :: (SingI m) => Bit n -> Bit m
bv_truncate x = bv_extract x 0

