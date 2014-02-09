
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Integer.Type (
   plusInteger, minusInteger, timesInteger,
   absInteger, signumInteger,
    ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger = {-# SCC "PRIM_PLUS_INTEGER" #-} (P.+)

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger = {-# SCC "PRIM_MINUS_INTEGER" #-} (P.-)

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger = {-# SCC "PRIM_TIMES_INTEGER" #-} (P.*)

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger = {-# SCC "PRIM_ABS_INTEGER" #-} P.abs

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger = {-# SCC "PRIM_SIGNUM_INTEGER" #-} P.signum

