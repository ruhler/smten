
module Smten.Runtime.Primitives (
    Char, Integer, Bool, Bit,
    primCharToInteger, primIntegerToChar,
    bv_make, bv_add, bv_sub, bv_mul, bv_eq, bv_leq, bv_show,
    ) where

import Smten.Bit

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

bv_add :: Bit -> Bit -> Bit
bv_add = (+)

bv_sub :: Bit -> Bit -> Bit
bv_sub = (-)

bv_mul :: Bit -> Bit -> Bit
bv_mul = (*)

bv_eq :: Bit -> Bit -> Bool
bv_eq = (==)

bv_leq :: Bit -> Bit -> Bool
bv_leq = (<=)

bv_show :: Bit -> String
bv_show = show

