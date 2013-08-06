
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Ord1 (
    int_compare, integer_leq,
 ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Ord0
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}


fromp :: P.Ordering -> Ordering
fromp P.LT = LT
fromp P.EQ = EQ
fromp P.GT = GT

int_compare :: Int -> Int -> Ordering
int_compare a b = fromp (P.compare a b)
  
integer_leq :: Integer -> Integer -> Bool
integer_leq a b = if a P.<= b then True else False

