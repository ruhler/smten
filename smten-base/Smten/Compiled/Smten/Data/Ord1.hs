
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module Smten.Compiled.Smten.Data.Ord1 (
    int_leq, integer_leq,
  ) where

import qualified Prelude as P
import GHC.Prim
import Smten.Compiled.Smten.Data.Bool0
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula

int_leq :: Int -> Int -> Bool
int_leq (I# a) (I# b) = {-# SCC "PRIM_INT_LEQ" #-} if (a <=# b) then __True else __False
int_leq a b = {-# SCC "PRIM_INT_LEQ" #-} (symapp2 P.$ \av bv ->
  if (av :: P.Int) P.<= bv
     then __True
     else __False) a b
  
integer_leq :: Integer -> Integer -> Bool
integer_leq = {-# SCC "PRIM_INTEGER_LEQ" #-} leq_Integer

