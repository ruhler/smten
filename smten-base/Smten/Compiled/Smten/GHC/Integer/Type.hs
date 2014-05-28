
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Smten.Compiled.Smten.GHC.Integer.Type (
   plusInteger, minusInteger, timesInteger,
   quotInteger, remInteger, divInteger, modInteger,
   divModInteger, quotRemInteger,
   absInteger, signumInteger, eqInteger, leInteger,
   smallInteger, integerToInt,
    ) where

import qualified GHC.Integer as P
import qualified Prelude as P
import Smten.Compiled.GHC.Types
import Smten.Runtime.Int
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula
import Smten.Runtime.Integer

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
smallInteger i = primIntApp (\x -> integerF (P.smallInteger x)) i

quotInteger :: Integer -> Integer -> Integer
quotInteger = {-# SCC "PRIM_QUOT_INTEGER" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Integer) `P.quotInteger` bv

remInteger :: Integer -> Integer -> Integer
remInteger = {-# SCC "PRIM_REM_INTEGER" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Integer) `P.remInteger` bv

divInteger :: Integer -> Integer -> Integer
divInteger = {-# SCC "PRIM_DIV_INTEGER" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Integer) `P.divInteger` bv

modInteger :: Integer -> Integer -> Integer
modInteger = {-# SCC "PRIM_MOD_INTEGER" #-} symapp2 P.$ \av bv -> tosym P.$ (av :: P.Integer) `P.modInteger` bv

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger a b = {-# SCC "PRIM_QUOTREM_INTEGER" #-} (# quotInteger a b, remInteger a b #)

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger a b  = {-# SCC "PRIM_DIVMOD_INTEGER" #-} (# divInteger a b, modInteger a b #)

integerToInt :: Integer -> Int#
integerToInt a = symapp (\x -> int# (P.integerToInt x)) a

