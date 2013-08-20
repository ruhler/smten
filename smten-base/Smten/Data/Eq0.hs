
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Eq0 (int_eq, integer_eq) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE int_eq #-}
int_eq :: Int -> Int -> Bool
int_eq = {-# SCC "PRIM_INT_EQ" #-} (P.==)

{-# NOINLINE integer_eq #-}
integer_eq :: Integer -> Integer -> Bool
integer_eq = {-# SCC "PRIM_INTEGER_EQ" #-} (P.==)

