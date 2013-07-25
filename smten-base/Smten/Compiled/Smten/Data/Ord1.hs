
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Ord1 (
    int_compare, integer_compare,
  ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Ord0
import Smten.Runtime.SymbolicOf

fromp :: P.Ordering -> Ordering
fromp P.LT = LT
fromp P.EQ = EQ
fromp P.GT = GT

int_compare :: Int -> Int -> Ordering
int_compare = symapp2 P.$ \a b -> 
    fromp (P.compare (a :: P.Int) b)
  
integer_compare :: Integer -> Integer -> Ordering
integer_compare (Integer a) (Integer b) = fromp (P.compare a b)

