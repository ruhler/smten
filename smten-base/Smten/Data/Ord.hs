
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.Ord (
    Ord(..),
    Ordering(..),
    comparing,
 ) where

import GHC.Prim
import GHC.Types hiding (Ordering)
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Eq
import Smten.Data.Ord0
import Smten.GHC.Integer.Type

instance Ord Int where
    (<)     = ltInt
    (<=)    = leInt
    (>=)    = geInt
    (>)     = gtInt

{-# INLINE gtInt #-}
{-# INLINE geInt #-}
{-# INLINE ltInt #-}
{-# INLINE leInt #-}
gtInt, geInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = x >#  y
(I# x) `geInt` (I# y) = x >=# y
(I# x) `ltInt` (I# y) = x <#  y
(I# x) `leInt` (I# y) = x <=# y


instance Ord Integer where
    (<=) = leqInteger

instance (Ord a, Ord b) => Ord (a, b) where
    (<=) (a, b) (c, d) = (a < c) || (a == c && b <= d)

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
    (<=) (a1, a2, a3) (b1, b2, b3) =
        (a1 < b1) || (a1 == b1 && (a2 < b2 || a2 == b2 && a3 <= b3))
    
instance (Ord a) => Ord [a] where
    (<=) [] [] = True
    (<=) [] (_:_) = True
    (<=) (_:_) [] = False
    (<=) (a:as) (b:bs) = a < b || (a == b && as <= bs)

