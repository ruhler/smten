
module Smten.Compiled.Smten.Data.Num0 (
   int_add, int_sub, int_mul, int_negate,
   int_abs, int_signum, int_fromInteger,

   integer_add, integer_sub, integer_mul, integer_negate,
   integer_abs, integer_signum,
    ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base

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
int_fromInteger (Integer x) = P.fromInteger x

integer_add :: Integer -> Integer -> Integer
integer_add (Integer a) (Integer b) = Integer (a P.+ b)

integer_sub :: Integer -> Integer -> Integer
integer_sub (Integer a) (Integer b) = Integer (a P.- b)

integer_mul :: Integer -> Integer -> Integer
integer_mul (Integer a) (Integer b) = Integer (a P.* b)

integer_negate :: Integer -> Integer
integer_negate (Integer x) = Integer (P.negate x)

integer_abs :: Integer -> Integer
integer_abs (Integer x) = Integer (P.abs x)

integer_signum :: Integer -> Integer
integer_signum (Integer x) = Integer (P.signum x)

