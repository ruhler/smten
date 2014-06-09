
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Integer.Type (
   Integer,
   plusInteger, minusInteger, timesInteger,
   quotInteger, remInteger, divInteger, modInteger,
   divModInteger, quotRemInteger,
   absInteger, signumInteger, eqInteger,
   leInteger, smallInteger, integerToInt,
    ) where

import GHC.Prim
import GHC.Types
import qualified GHC.Integer as P
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}


-- Note: the primitives are redefined here to avoid linking directly
-- to Smten.Compiled.GHC.Integer.Type, which would cause dependency problems.

data Integer = Integer P.Integer

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger (Integer a) (Integer b) = Integer (P.plusInteger a b)

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger (Integer a) (Integer b) = Integer (P.minusInteger a b)

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger (Integer a) (Integer b) = Integer (P.timesInteger a b)

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger (Integer a) = Integer (P.absInteger a)

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger (Integer a) = Integer (P.signumInteger a)

{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger (Integer a) (Integer b) = P.eqInteger a b

{-# NOINLINE leInteger #-}
leInteger :: Integer -> Integer -> Bool
leInteger (Integer a) (Integer b) = P.leInteger a b

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger x = Integer (P.smallInteger x)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (Integer a) = P.integerToInt a

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger (Integer a) (Integer b) = Integer (P.quotInteger a b)

{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger (Integer a) (Integer b) = Integer (P.remInteger a b)

{-# NOINLINE modInteger #-}
modInteger :: Integer -> Integer -> Integer
modInteger (Integer a) (Integer b) = Integer (P.modInteger a b)

{-# NOINLINE divInteger #-}
divInteger :: Integer -> Integer -> Integer
divInteger (Integer a) (Integer b) = Integer (P.divInteger a b)

{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger (Integer a) (Integer b) =
  case P.divModInteger a b of
    (# x, y #) -> (# Integer x, Integer y #)

{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (Integer a) (Integer b) =
  case P.quotRemInteger a b of
    (# x, y #) -> (# Integer x, Integer y #)

