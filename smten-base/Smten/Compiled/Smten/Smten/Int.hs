
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Int (
    Int(..), __I#, __applyToInt,
  ) where

import qualified Prelude as P
import qualified GHC.Types as P
import qualified GHC.Prim as P

import Smten.Runtime.Formula
import Smten.Runtime.Select
import Smten.Runtime.StableNameEq
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

data Int =
    I# P.Int#
  | Ite_Int BoolF Int Int
  | Unreachable_Int

__I# = I#

instance SymbolicOf P.Int Int where
    tosym (P.I# x) = I# x
    symapp f x = __applyToInt (\v -> f (P.I# v)) x

__applyToInt :: (SmtenHS0 a) => (P.Int# -> a) -> Int -> a
__applyToInt f x =
  case x of
    I# i -> f i
    Ite_Int p a b -> ite0 p (__applyToInt f a) (__applyToInt f b)
    Unreachable_Int -> unreachable
     

instance SmtenHS0 Int where
    ite0 p a b = 
        case (select a b, a, b) of
           (SRBoth, I# av, I# bv) | av P.==# bv -> a
           (SRBoth, _, _) | a `stableNameEq` b -> a
           (SRBoth, Unreachable_Int, _) -> b
           (SRBoth, _, Unreachable_Int) -> a
           (SRLeft, Unreachable_Int, _) -> b
           (SRRight, _, Unreachable_Int) -> a
           _ -> Ite_Int p a b
    unreachable0 = Unreachable_Int

instance P.Num Int where
    fromInteger = tosym P.. (P.fromInteger :: P.Integer -> P.Int)
    (+) = P.error "Smten Int P.Num (+) not supported"
    (*) = P.error "Smten Int P.Num (*) not supported"
    abs = P.error "Smten Int P.Num abs not supported"
    signum = P.error "Smten Int P.Num signum not supported"
