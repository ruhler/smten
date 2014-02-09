
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Num0 (
   int_add, int_sub, int_mul,
   int_abs, int_signum, int_fromInteger,
    ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE int_add #-}
int_add :: Int -> Int -> Int
int_add = {-# SCC "PRIM_INT_ADD" #-} (P.+)

{-# NOINLINE int_sub #-}
int_sub :: Int -> Int -> Int
int_sub = {-# SCC "PRIM_INT_SUB" #-} (P.-)

{-# NOINLINE int_mul #-}
int_mul :: Int -> Int -> Int
int_mul = {-# SCC "PRIM_INT_MUL" #-} (P.*)

{-# NOINLINE int_abs #-}
int_abs :: Int -> Int
int_abs = {-# SCC "PRIM_INT_ABS" #-} P.abs

{-# NOINLINE int_signum #-}
int_signum :: Int -> Int
int_signum = {-# SCC "PRIM_INT_SIGNUM" #-} P.signum

{-# NOINLINE int_fromInteger #-}
int_fromInteger :: Integer -> Int
int_fromInteger = {-# SCC "PRIM_INT_FROMINTEGER" #-} P.fromInteger

