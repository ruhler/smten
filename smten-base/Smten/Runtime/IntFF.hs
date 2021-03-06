
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Runtime.IntFF (
    IntFF(..), ite_IntFF, applyToIntFF',
    neq_IntFF, eq_IntFF,
    leq_IntFF, geq_IntFF, lt_IntFF, gt_IntFF,
    add_IntFF, sub_IntFF, mul_IntFF, negate_IntFF,
    quot_IntFF, rem_IntFF,
    isLit_IntFF,
    trace_IntFF, trace_CharFF,
  ) where 

import Data.Char
import GHC.Prim
import GHC.Types
import qualified Data.IntMap as M
import Smten.Runtime.Formula.BoolF
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.PartialF
import Smten.Runtime.SmtenHS
import Smten.Runtime.Trace

data IntFF =
   IntFF Int#
 | Symbolic_IntFF (M.IntMap BoolFF)
 | Unreachable_IntFF
  deriving (Show)

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

-- Version of applyToIntFF with SmtenHS context instead of Finite.
-- TODO: This is annoying. Must we have this?
-- TODO: Don't use this, because it has terrible performance!
applyToIntFF' :: (SmtenHS0 a) => (Int# -> a) -> IntFF -> a
applyToIntFF' f x =
  case x of
    IntFF i -> f i
    Symbolic_IntFF m ->
      let g [] = unreachable
          g ((I# k,v):xs) = ite0 (finiteF v) (f k) (g xs)
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
iii f (IntFF a) (Symbolic_IntFF m) = 
  let g (I# b) = I# (f a b)
  in Symbolic_IntFF (M.mapKeysWith orFF g m)
iii f (Symbolic_IntFF m) (IntFF b) = 
  let g (I# a) = I# (f a b)
  in Symbolic_IntFF (M.mapKeysWith orFF g m)
iii f (Symbolic_IntFF a) (Symbolic_IntFF b) =
  let g (I# ka, pa) =
        let g' (I# kb) = I# (f ka kb)
        in M.map (andFF pa) (M.mapKeysWith orFF g' b)
  in Symbolic_IntFF (M.unionsWith orFF (map g (M.assocs a)))

leq_IntFF :: IntFF -> IntFF -> BoolFF
leq_IntFF a b = geq_IntFF b a

lt_IntFF :: IntFF -> IntFF -> BoolFF
lt_IntFF a b = gt_IntFF b a

geq_IntFF :: IntFF -> IntFF -> BoolFF
geq_IntFF a b =
  case a of
    IntFF x -> 
      case b of
        IntFF y ->
           -- x and y are both concrete. Easy.
           boolFF (x >=# y)

        Symbolic_IntFF y -> 
           -- x is concrete, y is symbolic.
           -- Is there any y > x?
           --   if not, then all y <= x, so x >= y
           -- Note: we have this special case, because it's not always obvious
           -- that big_OR of all y predicates is True
           case M.lookupGT (I# x) y of
              Nothing -> trueFF
              _ -> -- Otherwise, find all y <= x, because for all
                   -- y <= x, x >= y.
                   case M.split (I# (x +# 1#)) y of
                      (low, _) -> orsFF (M.elems low)
        Unreachable_IntFF -> Unreachable_BoolFF
    Symbolic_IntFF x ->
      case b of
        IntFF y -> 
           -- x is symbolic, y is concrete.
           -- Is there any x < y? If not, for all x, x >= y
           case M.lookupLT (I# y) x of
              Nothing -> trueFF
              _ -> -- Otherwise: find all x such that x >= y.
                   case M.split (I# (y -# 1#)) x of
                      (_, high) -> orsFF (M.elems high)
        Symbolic_IntFF y -> gef falseFF (M.toAscList x) (M.toAscList y)
        Unreachable_IntFF -> Unreachable_BoolFF
    Unreachable_IntFF -> Unreachable_BoolFF

gt_IntFF :: IntFF -> IntFF -> BoolFF
gt_IntFF a b =
  case a of
    IntFF x -> 
      case b of
        IntFF y -> boolFF (x ># y)
        Symbolic_IntFF y ->
           case M.lookupGE (I# x) y of
             Nothing -> trueFF
             _ -> case M.split (I# x) y of
                    (low, _) -> orsFF (M.elems low)
        Unreachable_IntFF -> Unreachable_BoolFF
    Symbolic_IntFF x ->
      case b of
        IntFF y ->
           case M.lookupLE (I# y) x of
             Nothing -> trueFF
             _ -> case M.split (I# y) x of
                     (_, high) -> orsFF (M.elems high)
        Symbolic_IntFF y -> gtf falseFF (M.toAscList x) (M.toAscList y)
        Unreachable_IntFF -> Unreachable_BoolFF
    Unreachable_IntFF -> Unreachable_BoolFF

-- gtf p as bs
--   as - a list of (va, pa) uniquely ascending in va's
--         'a' has value va when 'pa' is satisfied
--   bs - a list of (vb, pb) uniquely ascending in vb's
--         'b' has value vb when 'pb' is satisfied
--   p - A formula which is satisfied for all values vb
--        which are less than the head 'va' and less than the head 'vb'
--   Produces: The boolean formula for (a > b)
gtf :: BoolFF -> [(Int, BoolFF)] -> [(Int, BoolFF)] -> BoolFF
gtf p [] _ = falseFF
gtf p as [] = p `andFF` (orsFF (map snd as)) -- All remaining as are greater than b when 'p' holds
gtf p a@((va, pa):as) b@((vb, pb):bs)
 | vb < va = gtf (p `orFF` pb) a bs
 | otherwise = (p `andFF` pa) `orFF` gtf p as b

-- gef p as bs
--   as - a list of (va, pa) uniquely ascending in va's
--         'a' has value va when 'pa' is satisfied
--   bs - a list of (vb, pb) uniquely ascending in vb's
--         'b' has value vb when 'pb' is satisfied
--   p - A formula which is satisfied for all values vb
--        which are less than or equal the head 'va' and less than the head 'vb'
--   Produces: The boolean formula for (a >= b)
gef :: BoolFF -> [(Int, BoolFF)] -> [(Int, BoolFF)] -> BoolFF
gef p [] _ = falseFF
gef p as [] = p `andFF` (orsFF (map snd as)) -- All remaining as are greater than b when 'p' holds
gef p a@((va, pa):as) b@((vb, pb):bs)
 | vb <= va = gef (p `orFF` pb) a bs
 | otherwise = (p `andFF` pa) `orFF` gef p as b

add_IntFF :: IntFF -> IntFF -> IntFF
add_IntFF = iii (+#)

sub_IntFF :: IntFF -> IntFF -> IntFF
sub_IntFF = iii (-#)

mul_IntFF :: IntFF -> IntFF -> IntFF
mul_IntFF = iii (*#)

negate_IntFF :: IntFF -> IntFF
negate_IntFF (IntFF a) = IntFF (negateInt# a)
negate_IntFF (Symbolic_IntFF m) =
  let g (I# a) = I# (negateInt# a)
  in Symbolic_IntFF (M.mapKeys g m)
negate_IntFF Unreachable_IntFF = Unreachable_IntFF

-- TODO: We should not crash if the second argument is 0, because it may be
-- speculative execution. Really we should return an explict error. For now
-- I'm just returning 0#.
quot_IntFF :: IntFF -> IntFF -> IntFF
quot_IntFF = iii (\a b -> if b ==# 0# then 0# else quotInt# a b)

-- TODO: We should not crash if the second argument is 0, because it may be
-- speculative execution. Really we should return an explict error. For now
-- I'm just returning 0#.
rem_IntFF :: IntFF -> IntFF -> IntFF
rem_IntFF = iii (\a b -> if b ==# 0# then 0# else remInt# a b)

trace_IntFF :: IntFF -> String
trace_IntFF (IntFF i) = show (I# i)
trace_IntFF (Symbolic_IntFF m) =
  let f :: (Int, BoolFF) -> (String, BoolF, [String])
      f (i, v) = (show i, finiteF v, [])
  in traceSD "IntFF" (map f (M.assocs m))
trace_IntFF Unreachable_IntFF = "UR"

-- Trace an IntFF under, interpreted as a character.
trace_CharFF :: IntFF -> String
trace_CharFF (IntFF i) = show (C# (chr# i))
trace_CharFF (Symbolic_IntFF m) =
  let f :: (Int, BoolFF) -> (String, BoolF, [String])
      f (i, v) = (show (chr i), finiteF v, [])
  in traceSD "CharFF" (map f (M.assocs m))
trace_CharFF Unreachable_IntFF = "UR"

