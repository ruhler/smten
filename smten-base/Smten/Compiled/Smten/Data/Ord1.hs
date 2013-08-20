
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Ord1 (
    int_compare, integer_leq,
  ) where

import qualified Prelude as P
import GHC.Prim
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Ord0
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Types

fromp :: P.Ordering -> Ordering
fromp P.LT = LT
fromp P.EQ = EQ
fromp P.GT = GT

int_compare :: Int -> Int -> Ordering
int_compare (I# a) (I# b) = {-# SCC "PRIM_INT_COMPARE" #-}
  if (a ==# b)
     then EQ
     else if (a <=# b) then LT else GT
int_compare ai bi = {-# SCC "PRIM_INT_COMPARE" #-} (symapp2 P.$ \a b -> 
    fromp (P.compare (a :: P.Int) b)) ai bi 
  
integer_leq :: Integer -> Integer -> Bool
integer_leq = {-# SCC "PRIM_INTEGER_LEQ" #-} leq_Integer

