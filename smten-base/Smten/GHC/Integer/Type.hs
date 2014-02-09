
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Integer.Type (
   integer_add, integer_sub, integer_mul,
   integer_abs, integer_signum,
    ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE integer_add #-}
integer_add :: Integer -> Integer -> Integer
integer_add = {-# SCC "PRIM_INTEGER_ADD" #-} (P.+)

{-# NOINLINE integer_sub #-}
integer_sub :: Integer -> Integer -> Integer
integer_sub = {-# SCC "PRIM_INTEGER_SUB" #-} (P.-)

{-# NOINLINE integer_mul #-}
integer_mul :: Integer -> Integer -> Integer
integer_mul = {-# SCC "PRIM_INTEGER_MUL" #-} (P.*)

{-# NOINLINE integer_abs #-}
integer_abs :: Integer -> Integer
integer_abs = {-# SCC "PRIM_INTEGER_ABS" #-} P.abs

{-# NOINLINE integer_signum #-}
integer_signum :: Integer -> Integer
integer_signum = {-# SCC "PRIM_INTEGER_SIGNUM" #-} P.signum

