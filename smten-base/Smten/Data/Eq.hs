
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Eq (Eq(..)) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool

infix 4 ==, /=

class Eq a where
    (==) :: a -> a -> Bool
    (==) x y = not (x /= y)

    (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)

instance Eq Int where
    (==) a b = if a P.== b then True else False

instance Eq Integer where
    (==) a b = if a P.== b then True else False
