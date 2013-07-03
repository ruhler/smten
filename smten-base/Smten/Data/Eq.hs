
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Eq (Eq(..)) where

import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Eq0

infix 4 ==, /=

class Eq a where
    (==) :: a -> a -> Bool
    (==) x y = not (x /= y)

    (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)

instance Eq Int where
    (==) = int_eq

instance Eq Integer where
    (==) = integer_eq

