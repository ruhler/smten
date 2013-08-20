
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.Smten.Data.Num0 (
   int_add, int_sub, int_mul,
   int_abs, int_signum, int_fromInteger,

   integer_add, integer_sub, integer_mul,
   integer_abs, integer_signum,
    ) where

import qualified Prelude as P
import GHC.Prim
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Types

int_add :: Int -> Int -> Int
int_add (I# a) (I# b) = {-# SCC "PRIM_INT_ADD" #-} I# (a +# b)
int_add a b = {-# SCC "PRIM_INT_ADD" #-} (symapp2 P.$ \av bv ->
    tosym P.$ (av :: P.Int) P.+ bv) a b

int_sub :: Int -> Int -> Int
int_sub = {-# SCC "PRIM_INT_SUB" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Int) P.- bv

int_mul :: Int -> Int -> Int
int_mul = {-# SCC "PRIM_INT_MUL" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Int) P.* bv

int_abs :: Int -> Int
int_abs = {-# SCC "PRIM_INT_ABS" #-} symapp (tosym P.. (P.abs :: P.Int -> P.Int))

int_signum :: Int -> Int
int_signum = {-# SCC "PRIM_INT_SIGNUM" #-} symapp (tosym P.. (P.signum :: P.Int -> P.Int))

int_fromInteger :: Integer -> Int
int_fromInteger (Integer x) = {-# SCC "PRIM_INT_FROMINTEGER" #-} P.fromInteger x

integer_add :: Integer -> Integer -> Integer
integer_add = {-# SCC "PRIM_INTEGER_ADD" #-} add_Integer

integer_sub :: Integer -> Integer -> Integer
integer_sub = {-# SCC "PRIM_INTEGER_SUB" #-} sub_Integer

integer_mul :: Integer -> Integer -> Integer
integer_mul = {-# SCC "PRIM_INTEGER_MUL" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Integer) P.* bv

integer_abs :: Integer -> Integer
integer_abs = {-# SCC "PRIM_INTEGER_ABS" #-} symapp (tosym P.. (P.abs :: P.Integer -> P.Integer))

integer_signum :: Integer -> Integer
integer_signum = {-# SCC "PRIM_INTEGER_SIGNUM" #-} symapp (tosym P.. (P.signum :: P.Integer -> P.Integer))

