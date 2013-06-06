
module Smten.Runtime.Primitives (
    Char, Integer, Bool, Bit,
    primCharToInteger, primIntegerToChar,
    bv_make, show,
    ) where

import Smten.Bit

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

