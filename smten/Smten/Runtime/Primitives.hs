
module Smten.Runtime.Primitives (
    Char, Integer, Bool,
    primCharToInteger, primIntegerToChar,
    ) where

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

