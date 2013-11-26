
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Eq0 (int_eq, integer_eq) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool0
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula
import GHC.Prim

int_eq :: Int -> Int -> Bool
int_eq (I# a) (I# b) = {-# SCC "PRIM_INT_EQ" #-} if (a ==# b) then True else False
int_eq a b = {-# SCC "PRIM_INT_EQ" #-} (symapp2 P.$ \av bv ->
    if (av :: P.Int) P.== bv    
        then True
        else False) a b

integer_eq :: Integer -> Integer -> Bool
integer_eq = {-# SCC "PRIM_INTEGER_EQ" #-} eq_Integer

