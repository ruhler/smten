
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Eq0 (integer_eq) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE integer_eq #-}
integer_eq :: Integer -> Integer -> Bool
integer_eq = {-# SCC "PRIM_INTEGER_EQ" #-} (P.==)

