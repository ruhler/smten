
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Num0 (
    int_fromInteger,
    ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE int_fromInteger #-}
int_fromInteger :: Integer -> Int
int_fromInteger = {-# SCC "PRIM_INT_FROMINTEGER" #-} P.fromInteger

