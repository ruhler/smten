
module Smten.Runtime.Primitives (
    Char, Integer, Bool, Bit,
    primCharToInteger, primIntegerToChar,
    bv_make, bv_width, bv_value, show, trace,
    ) where

import Debug.Trace
import Smten.Bit

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

