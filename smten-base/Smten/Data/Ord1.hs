
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Ord1 (
    int_compare, integer_compare
 ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Ord0
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}


fromp :: P.Ordering -> Ordering
fromp P.LT = LT
fromp P.EQ = EQ
fromp P.GT = GT

int_compare :: Int -> Int -> Ordering
int_compare a b = fromp (P.compare a b)
  
integer_compare :: Integer -> Integer -> Ordering
integer_compare a b = fromp (P.compare a b)

