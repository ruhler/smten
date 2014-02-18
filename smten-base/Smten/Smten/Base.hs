
module Smten.Smten.Base (
    Char, String,
    Int, Integer,
    error,
    ) where

import qualified Prelude as P
import Prelude (Integer, String)
import Smten.Plugin.Annotations
import Smten.Smten.Char
import Smten.Smten.Int
import Smten.Smten.Integer
import Smten.Smten.List ()
import Smten.Smten.Tuple ()
import Smten.Smten.Unit ()

{-# ANN module PrimitiveModule #-}

{-# NOINLINE error #-}
error :: String -> a
error = {-# SCC "PRIM_ERROR" #-} P.error

