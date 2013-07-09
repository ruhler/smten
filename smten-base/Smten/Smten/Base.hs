
module Smten.Smten.Base (
    Char, String,
    Int, Integer,
    error, undefined,
    ) where

import qualified Prelude as P
import Prelude (Char, Int, Integer)
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

type String = [Char]

error :: String -> a
error = P.error

undefined :: a
undefined = error "Prelude.undefined"

