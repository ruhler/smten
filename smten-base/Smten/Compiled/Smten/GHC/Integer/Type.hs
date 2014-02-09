
module Smten.Compiled.Smten.GHC.Integer.Type (
   plusInteger, minusInteger, timesInteger,
   absInteger, signumInteger, eqInteger, leqInteger,
    ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool0
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula

plusInteger :: Integer -> Integer -> Integer
plusInteger = {-# SCC "PRIM_PLUS_INTEGER" #-} add_IntegerF

minusInteger :: Integer -> Integer -> Integer
minusInteger = {-# SCC "PRIM_MINUS_INTEGER" #-} sub_IntegerF

timesInteger :: Integer -> Integer -> Integer
timesInteger = {-# SCC "PRIM_TIMES_INTEGER" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Integer) P.* bv

absInteger :: Integer -> Integer
absInteger = {-# SCC "PRIM_ABS_INTEGER" #-} symapp (tosym P.. (P.abs :: P.Integer -> P.Integer))

signumInteger :: Integer -> Integer
signumInteger = {-# SCC "PRIM_SIGNUM_INTEGER" #-} symapp (tosym P.. (P.signum :: P.Integer -> P.Integer))

eqInteger :: Integer -> Integer -> Bool
eqInteger = {-# SCC "PRIM_EQ_INTEGER" #-} eq_IntegerF

leqInteger :: Integer -> Integer -> Bool
leqInteger = {-# SCC "PRIM_LEQ_INTEGER" #-} leq_IntegerF

