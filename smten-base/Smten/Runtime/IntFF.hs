
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Runtime.IntFF (
    IntFF(..), ite_IntFF, applyToIntFF, applyToIntFF',
    neq_IntFF, eq_IntFF,
    leq_IntFF, geq_IntFF, lt_IntFF, gt_IntFF,
    add_IntFF, sub_IntFF, mul_IntFF, negate_IntFF,
    quot_IntFF, rem_IntFF,
    isLit_IntFF,
  ) where

import GHC.Prim
import GHC.Types
import qualified Data.IntMap as M
import Smten.Runtime.Formula.BoolF
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.PartialF
import Smten.Runtime.SmtenHS

data IntFF =
   IntFF Int#
 | Symbolic_IntFF (M.IntMap BoolFF)
 | Unreachable_IntFF

ite_IntFF :: BoolFF -> IntFF -> IntFF -> IntFF 
ite_IntFF TrueFF a _ = a
ite_IntFF FalseFF _ b = b
ite_IntFF Unreachable_BoolFF _ _ = Unreachable_IntFF
ite_IntFF p v@(IntFF a) (IntFF b) | a ==# b = v
ite_IntFF _ Unreachable_IntFF b = b
ite_IntFF _ a Unreachable_IntFF = a
ite_IntFF p a b =
  let ma = case a of
             IntFF av -> M.singleton (I# av) trueFF
             Symbolic_IntFF m -> m
      mb = case b of
             IntFF bv -> M.singleton (I# bv) trueFF
             Symbolic_IntFF m -> m
      f _ va vb = Just (iteFF p va vb)
      o1 ma = M.map (andFF p) ma
      o2 mb = M.map (andFF (notFF p)) mb
  in Symbolic_IntFF (M.mergeWithKey f o1 o2 ma mb)

instance Finite IntFF where
    ite_finite = ite_IntFF
    unreachable_finite = Unreachable_IntFF

applyToIntFF :: (Finite a) => (Int# -> a) -> IntFF -> a
applyToIntFF f x =
  case x of
    IntFF i -> f i
    Symbolic_IntFF m ->
      let g [] = unreachable_finite
          g ((I# k,v):xs) = ite_finite v (f k) (g xs)
      in g (M.assocs m)
    Unreachable_IntFF -> unreachable_finite


-- Version of applyToIntFF with SmtenHS context instead of Finite.
-- TODO: This is annoying. Must we have this?
applyToIntFF' :: (SmtenHS0 a) => (Int# -> a) -> IntFF -> a
applyToIntFF' f x =
  case x of
    IntFF i -> f i
    Symbolic_IntFF m ->
      let g [] = unreachable
          g ((I# k,v):xs) = ite (finiteF v) (f k) (g xs)
      in g (M.assocs m)
    Unreachable_IntFF -> unreachable

neq_IntFF :: IntFF -> IntFF -> BoolFF
neq_IntFF a b = notFF (eq_IntFF a b)

eq_IntFF :: IntFF -> IntFF -> BoolFF
eq_IntFF a b =
  case a of
     IntFF x -> isLit_IntFF x b
     Symbolic_IntFF x ->
        case b of
          IntFF y -> M.findWithDefault falseFF (I# y) x
          Symbolic_IntFF y -> M.foldr orFF falseFF (M.intersectionWith andFF x y)
          Unreachable_IntFF -> Unreachable_BoolFF
     Unreachable_IntFF -> Unreachable_BoolFF

isLit_IntFF :: Int# -> IntFF -> BoolFF
isLit_IntFF x b =
  case b of
    IntFF y -> boolFF (x ==# y)
    Symbolic_IntFF y -> M.findWithDefault falseFF (I# x) y
    Unreachable_IntFF -> Unreachable_BoolFF

iii :: (Int# -> Int# -> Int#) -> IntFF -> IntFF -> IntFF
iii f (IntFF a) (IntFF b) = IntFF (f a b)
iii f Unreachable_IntFF _ = Unreachable_IntFF
iii f _ Unreachable_IntFF = Unreachable_IntFF
iii f a b = applyToIntFF (\x -> applyToIntFF (\y -> IntFF (f x y))) a b

iib :: (Int# -> Int# -> Bool) -> IntFF -> IntFF -> BoolFF
iib f (IntFF a) (IntFF b) = boolFF (f a b)
iib f Unreachable_IntFF _ = Unreachable_BoolFF
iib f _ Unreachable_IntFF = Unreachable_BoolFF
iib f a b = applyToIntFF (\x -> applyToIntFF (\y -> boolFF (f x y))) a b

leq_IntFF :: IntFF -> IntFF -> BoolFF
leq_IntFF = iib (<=#)

geq_IntFF :: IntFF -> IntFF -> BoolFF
geq_IntFF = iib (>=#)

lt_IntFF :: IntFF -> IntFF -> BoolFF
lt_IntFF = iib (<#)

gt_IntFF :: IntFF -> IntFF -> BoolFF
gt_IntFF = iib (>#)

add_IntFF :: IntFF -> IntFF -> IntFF
add_IntFF = iii (+#)

sub_IntFF :: IntFF -> IntFF -> IntFF
sub_IntFF = iii (-#)

mul_IntFF :: IntFF -> IntFF -> IntFF
mul_IntFF = iii (*#)

negate_IntFF :: IntFF -> IntFF
negate_IntFF a = applyToIntFF (\x -> IntFF (negateInt# x)) a

quot_IntFF :: IntFF -> IntFF -> IntFF
quot_IntFF = iii quotInt#

rem_IntFF :: IntFF -> IntFF -> IntFF
rem_IntFF = iii remInt#

