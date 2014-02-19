
module Smten.Data.Integral0 (
    int_quot, int_rem,
    ) where

import qualified Prelude as P
import Smten.Plugin.Annotations
import Smten.Smten.Int

{-# ANN module PrimitiveModule #-}

{-# NOINLINE int_quot #-}
int_quot :: Int -> Int -> Int
int_quot = {-# SCC "PRIM_INT_QUOT" #-} P.quot

{-# NOINLINE int_rem #-}
int_rem :: Int -> Int -> Int
int_rem = {-# SCC "PRIM_INT_REM" #-} P.rem

