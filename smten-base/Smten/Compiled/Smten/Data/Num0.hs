
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.Smten.Data.Num0 (
   int_add, int_sub, int_mul,
   int_abs, int_signum, int_fromInteger,
    ) where

import qualified Prelude as P
import GHC.Prim
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf

int_add :: Int -> Int -> Int
int_add (I# a) (I# b) = {-# SCC "PRIM_INT_ADD" #-} I# (a +# b)
int_add a b = {-# SCC "PRIM_INT_ADD" #-} (symapp2 P.$ \av bv ->
    tosym P.$ (av :: P.Int) P.+ bv) a b

int_sub :: Int -> Int -> Int
int_sub (I# a) (I# b) = {-# SCC "PRIM_INT_SUB" #-} I# (a -# b)
int_sub a b = {-# SCC "PRIM_INT_SUB" #-} (symapp2 P.$ \av bv ->
    tosym P.$ (av :: P.Int) P.- bv) a b

int_mul :: Int -> Int -> Int
int_mul (I# a) (I# b) = {-# SCC "PRIM_INT_MUL" #-} I# (a *# b)
int_mul a b = {-# SCC "PRIM_INT_MUL" #-} (symapp2 P.$ \av bv ->
    tosym P.$ (av :: P.Int) P.* bv) a b

int_abs :: Int -> Int
int_abs = {-# SCC "PRIM_INT_ABS" #-} symapp (tosym P.. (P.abs :: P.Int -> P.Int))

int_signum :: Int -> Int
int_signum = {-# SCC "PRIM_INT_SIGNUM" #-} symapp (tosym P.. (P.signum :: P.Int -> P.Int))

int_fromInteger :: Integer -> Int
int_fromInteger = {-# SCC "PRIM_INT_FROMINTEGER" #-}  symapp P.fromInteger

