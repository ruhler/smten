
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.EnumInteger () where

import Smten.Smten.Base
import Smten.GHC.Enum
import Smten.Data.Ord
import Smten.Data.Integral
import GHC.Num(Num(..))

instance Enum Integer where
    succ x = x + 1
    pred x = x - 1
    toEnum = toInteger
    fromEnum = fromInteger

    enumFrom i = i : enumFrom (i+1)
    enumFromThen a b = a : enumFromThen b (b + b - a)
    enumFromTo a b = if a > b then [] else a : enumFromTo (a+1) b
    enumFromThenTo a b c =
        if a > c
            then []
            else a : enumFromThenTo b (b + b - a) c
    
