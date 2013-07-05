
{-# LANGUAGE RebindableSyntax #-}
module Smten.Data.Num (
    Num(..)
     ) where

import Smten.Smten.Base
import Smten.Data.Eq
import Smten.Data.Show
import Smten.Data.Num0

infixl 7 *
infixl 6 +, -

class (Eq a, Show a) => Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (-) x y = x + negate y
    (*) :: a -> a -> a
    negate :: a -> a
    negate x = 0 - x
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a

instance Num Int where
    (+) = int_add
    (-) = int_sub
    (*) = int_mul
    negate = int_negate
    abs = int_abs
    signum = int_signum
    fromInteger = int_fromInteger

instance Num Integer where
    (+) = integer_add
    (-) = integer_sub
    (*) = integer_mul
    negate = integer_negate
    abs = integer_abs
    signum = integer_signum
    fromInteger x = x
    
