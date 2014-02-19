
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Enum (Enum(..)) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Enum instead of Smten.Compiled.Smten.GHC.Enum

import GHC.Base
import GHC.Num
import Smten.Data.Integral0 (int_toInteger)

class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int

    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

instance Enum Int where
    succ x = x + 1
    pred x = x - 1
    toEnum = id
    fromEnum = id

    enumFrom i = i : enumFrom (i+1)
    enumFromThen a b = a : enumFromThen b (b + b - a)
    enumFromTo a b = if a > b then [] else a : enumFromTo (a+1) b
    enumFromThenTo a b c =
        if a > c
            then []
            else a : enumFromThenTo b (b + b - a) c
    
instance Enum Integer where
    succ x = x + 1
    pred x = x - 1
    toEnum = int_toInteger
    fromEnum = fromInteger

    enumFrom i = i : enumFrom (i+1)
    enumFromThen a b = a : enumFromThen b (b + b - a)
    enumFromTo a b = if a > b then [] else a : enumFromTo (a+1) b
    enumFromThenTo a b c =
        if a > c
            then []
            else a : enumFromThenTo b (b + b - a) c
    
