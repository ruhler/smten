
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.Ord (
    Ord(..),
    Ordering(..),
    comparing,
 ) where

import Smten.Smten.Base
import Smten.Data.Ord0
import Smten.Data.Ord1

instance Ord Int where
    compare = int_compare

instance Ord Integer where
    compare = integer_compare

