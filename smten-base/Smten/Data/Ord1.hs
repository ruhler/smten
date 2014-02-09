
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Ord1 (
    int_leq,
 ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE int_leq #-}
int_leq :: Int -> Int -> Bool
int_leq = {-# SCC "PRIM_INT_LEQ" #-} (P.<=)
  
