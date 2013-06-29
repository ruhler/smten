
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RebindableSyntax #-}
module Smten.Data.Num (
    Num(..)
     ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Eq
import Smten.Data.Show

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
    (+) = (P.+)
    (-) = (P.-)
    (*) = (P.*)
    negate = P.negate
    abs = P.abs
    signum = P.signum
    fromInteger = P.fromInteger

instance Num Integer where
    (+) = (P.+)
    (-) = (P.-)
    (*) = (P.*)
    negate = P.negate
    abs = P.abs
    signum = P.signum
    fromInteger x = x
    
