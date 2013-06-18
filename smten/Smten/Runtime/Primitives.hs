
module Smten.Runtime.Primitives (
    Char, Integer, Bool,
    primCharToInteger, primIntegerToChar,
    error, trace,
    ) where

import Prelude hiding (error)
import Debug.Trace
import qualified Smten.Runtime.SmtenHS as S

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

error :: (S.SmtenHS0 a) => String -> a
error = S.error0 . S.errstr

