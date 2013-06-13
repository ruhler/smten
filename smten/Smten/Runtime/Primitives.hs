
module Smten.Runtime.Primitives (
    Char, Integer, Bool,
    primCharToInteger, primIntegerToChar,
    trace,
    ) where

import Debug.Trace

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

