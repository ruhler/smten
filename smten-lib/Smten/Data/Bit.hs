
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit (
    Bit
    ) where

import Smten.Prelude
import Smten.Data.Bit0
import Smten.Data.NumT

instance (Numeric n) => Eq (Bit n) where
    (==) = bv_eq

instance (Numeric n) => Show (Bit n) where
    show = bv_show

instance (Numeric n) => Num (Bit n) where
    (+) = bv_add
    (-) = bv_sub
    (*) = error "TODO: Bit.*"
    abs = error "TODO: Bit.abs"
    signum = error "TODO: Bit.signum"
    fromInteger = bv_fromInteger

