
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Integral (
    Integral(..),
    ) where

import GHC.Enum
import GHC.Types
import Smten.GHC.Integer.Type
import Smten.Smten.Integer
import Smten.Data.Integral0

class (Enum a) => Integral a where
    quot :: a -> a -> a
    quot n d = q where (q, _) = quotRem n d
    
    rem :: a -> a -> a
    rem n d = r where (_, r) = quotRem n d

    quotRem :: a -> a -> (a, a)
    toInteger :: a -> Integer

instance Integral Int where
    toInteger (I# i) = smallInteger i
    quotRem a b = (quot a b, rem a b)
    quot = int_quot
    rem = int_rem

