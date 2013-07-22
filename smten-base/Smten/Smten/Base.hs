
module Smten.Smten.Base (
    Char, String,
    Int, Integer,
    error, undefined,
    ) where

import qualified Prelude as P
import Prelude (Char, Int, Integer)
import Smten.Plugin.Annotations
import Smten.Smten.List ()
import Smten.Smten.Tuple ()
import Smten.Smten.Unit ()

{-# ANN module PrimitiveModule #-}

type String = [Char]

error :: String -> a
error = P.error

undefined :: a
undefined = error "Prelude.undefined"

