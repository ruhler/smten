
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.Ord (
    Ord(..),
    Ordering(..),
    comparing,
 ) where

import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Eq
import Smten.Data.Ord0
import Smten.Data.Ord1

instance Ord Int where
    compare = int_compare

instance Ord Integer where
    (<=) = integer_leq

instance (Ord a, Ord b) => Ord (a, b) where
    (<=) (a, b) (c, d) = (a < c) || (a == c && b <= d)

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
    (<=) (a1, a2, a3) (b1, b2, b3) =
        (a1 < b1) || (a1 == b1 && (a2 < b2 || a2 == b2 && a3 <= b3))
    

