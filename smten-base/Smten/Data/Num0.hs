
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Num0 (
   int_add, int_sub, int_mul, int_negate,
   int_abs, int_signum, int_fromInteger,

   integer_add, integer_sub, integer_mul, integer_negate,
   integer_abs, integer_signum,
    ) where

import qualified Prelude as P
import Smten.Smten.Base

int_add :: Int -> Int -> Int
int_add = (P.+)

int_sub :: Int -> Int -> Int
int_sub = (P.-)

int_mul :: Int -> Int -> Int
int_mul = (P.*)

int_negate :: Int -> Int
int_negate = P.negate

int_abs :: Int -> Int
int_abs = P.abs

int_signum :: Int -> Int
int_signum = P.signum

int_fromInteger :: Integer -> Int
int_fromInteger = P.fromInteger

integer_add :: Integer -> Integer -> Integer
integer_add = (P.+)

integer_sub :: Integer -> Integer -> Integer
integer_sub = (P.-)

integer_mul :: Integer -> Integer -> Integer
integer_mul = (P.*)

integer_negate :: Integer -> Integer
integer_negate = P.negate

integer_abs :: Integer -> Integer
integer_abs = P.abs

integer_signum :: Integer -> Integer
integer_signum = P.signum
