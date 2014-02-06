
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Integral (
    Integral(..),
    ) where

import Smten.Smten.Integer
import Smten.Smten.Int
import Smten.Data.Enum
import Smten.Data.EnumInt ()
import Smten.Data.Integral0

class (Enum a) => Integral a where
    quot :: a -> a -> a
    quot n d = q where (q, _) = quotRem n d
    
    rem :: a -> a -> a
    rem n d = r where (_, r) = quotRem n d

    quotRem :: a -> a -> (a, a)
    toInteger :: a -> Integer

instance Integral Int where
    quotRem a b = (quot a b, rem a b)
    quot = int_quot
    rem = int_rem
    toInteger = int_toInteger

