
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Enum (Enum(..)) where

import Smten.Smten.Base

class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int

    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

