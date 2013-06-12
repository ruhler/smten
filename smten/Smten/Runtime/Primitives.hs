
module Smten.Runtime.Primitives (
    Char, Integer, Bool, Bit, error,
    primCharToInteger, primIntegerToChar,
    bv_make, bv_width, bv_value, show, trace,
    ) where

import Prelude hiding (error)
import Debug.Trace
import Smten.Bit
import qualified Smten.Runtime.SmtenHS as S

primCharToInteger :: Char -> Integer
primCharToInteger = toInteger . fromEnum

primIntegerToChar :: Integer -> Char
primIntegerToChar = toEnum . fromInteger

error :: (S.SmtenHS0 a) => String -> a
error = S.error0

