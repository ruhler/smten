
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Num0 (
   int_add, int_sub, int_mul,
   int_abs, int_signum, int_fromInteger,

   integer_add, integer_sub, integer_mul,
   integer_abs, integer_signum,
    ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE int_add #-}
int_add :: Int -> Int -> Int
int_add = (P.+)

{-# NOINLINE int_sub #-}
int_sub :: Int -> Int -> Int
int_sub = (P.-)

{-# NOINLINE int_mul #-}
int_mul :: Int -> Int -> Int
int_mul = (P.*)

{-# NOINLINE int_abs #-}
int_abs :: Int -> Int
int_abs = P.abs

{-# NOINLINE int_signum #-}
int_signum :: Int -> Int
int_signum = P.signum

{-# NOINLINE int_fromInteger #-}
int_fromInteger :: Integer -> Int
int_fromInteger = P.fromInteger

{-# NOINLINE integer_add #-}
integer_add :: Integer -> Integer -> Integer
integer_add = (P.+)

{-# NOINLINE integer_sub #-}
integer_sub :: Integer -> Integer -> Integer
integer_sub = (P.-)

{-# NOINLINE integer_mul #-}
integer_mul :: Integer -> Integer -> Integer
integer_mul = (P.*)

{-# NOINLINE integer_abs #-}
integer_abs :: Integer -> Integer
integer_abs = P.abs

{-# NOINLINE integer_signum #-}
integer_signum :: Integer -> Integer
integer_signum = P.signum

