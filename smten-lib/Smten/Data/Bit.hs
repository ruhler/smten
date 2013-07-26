
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit (
    Bit
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
    (*) = error "TODO: Bit.*"
    abs = error "TODO: Bit.abs"
    signum = error "TODO: Bit.signum"
    fromInteger = bv_fromInteger

