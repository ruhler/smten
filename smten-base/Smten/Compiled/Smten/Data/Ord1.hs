
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Ord1 (
    int_compare, integer_leq,
  ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Ord0
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Types

fromp :: P.Ordering -> Ordering
fromp P.LT = LT
fromp P.EQ = EQ
fromp P.GT = GT

int_compare :: Int -> Int -> Ordering
int_compare = symapp2 P.$ \a b -> 
    fromp (P.compare (a :: P.Int) b)
  
integer_leq :: Integer -> Integer -> Bool
integer_leq = leq_Integer

