
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Enum (Enum(..)) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Enum instead of Smten.Compiled.Smten.GHC.Enum

import GHC.Base
import GHC.Char
import GHC.Num

class Bounded a where
    minBound, maxBound :: a

class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int

    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

instance Bounded () where
    minBound = ()
    maxBound = ()

instance Bounded Bool where
    minBound = False
    maxBound = True

instance Bounded Ordering where
    minBound = LT
    maxBound = GT

instance Bounded Int where
    minBound = minInt
    maxBound = maxInt

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

instance Enum Char where
    succ x = chr (ord x + 1)
    pred x = chr (ord x - 1)
    toEnum = chr
    fromEnum = ord

    enumFrom i = map toEnum (enumFrom (fromEnum i))
    enumFromThen a b = map toEnum (enumFromThen (fromEnum a) (fromEnum b))
    enumFromTo a b = map toEnum (enumFromTo (fromEnum a) (fromEnum b))
    enumFromThenTo a b c = map toEnum (enumFromThenTo (fromEnum a) (fromEnum b) (fromEnum c))
    
instance Enum Integer where
    succ x = x + 1
    pred x = x - 1
    toEnum (I# i) = smallInteger i
    fromEnum = fromInteger

    enumFrom i = i : enumFrom (i+1)
    enumFromThen a b = a : enumFromThen b (b + b - a)
    enumFromTo a b = if a > b then [] else a : enumFromTo (a+1) b
    enumFromThenTo a b c =
        if a > c
            then []
            else a : enumFromThenTo b (b + b - a) c
    
