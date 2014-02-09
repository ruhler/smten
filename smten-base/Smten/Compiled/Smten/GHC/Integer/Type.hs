
module Smten.Compiled.Smten.GHC.Integer.Type (
   integer_add, integer_sub, integer_mul,
   integer_abs, integer_signum,
    ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula

integer_add :: Integer -> Integer -> Integer
integer_add = {-# SCC "PRIM_INTEGER_ADD" #-} add_IntegerF

integer_sub :: Integer -> Integer -> Integer
integer_sub = {-# SCC "PRIM_INTEGER_SUB" #-} sub_IntegerF

integer_mul :: Integer -> Integer -> Integer
integer_mul = {-# SCC "PRIM_INTEGER_MUL" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Integer) P.* bv

integer_abs :: Integer -> Integer
integer_abs = {-# SCC "PRIM_INTEGER_ABS" #-} symapp (tosym P.. (P.abs :: P.Integer -> P.Integer))

integer_signum :: Integer -> Integer
integer_signum = {-# SCC "PRIM_INTEGER_SIGNUM" #-} symapp (tosym P.. (P.signum :: P.Integer -> P.Integer))

