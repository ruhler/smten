
module Smten.Runtime.Primitives (
    Char, Integer, primCharToInteger, primIntegerToChar,
    ) where

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

