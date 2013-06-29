
module Smten.Smten.Base (
    Char, String,
    Int, Integer,
    error, undefined,
    ) where

import qualified Prelude as P

type Char = P.Char
type Int = P.Int
type Integer = P.Integer
type String = [Char]

error :: String -> a
error = P.error

undefined :: a
undefined = error "Prelude.undefined"

