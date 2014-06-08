
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.Ord (
    Ord(..),
    Ordering(..),
    comparing,
 ) where

import Prelude(Ord(..), Ordering(..))

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

