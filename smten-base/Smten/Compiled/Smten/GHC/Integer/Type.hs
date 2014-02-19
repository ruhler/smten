
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.Smten.GHC.Integer.Type (
   plusInteger, minusInteger, timesInteger,
   absInteger, signumInteger, eqInteger, leInteger,
   smallInteger,
    ) where

import GHC.Prim
import qualified GHC.Integer as P
import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.GHC.Types
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

leInteger :: Integer -> Integer -> Bool
leInteger = {-# SCC "PRIM_LE_INTEGER" #-} leq_IntegerF

smallInteger :: Int# -> Integer
smallInteger i = integerF (P.smallInteger i)

