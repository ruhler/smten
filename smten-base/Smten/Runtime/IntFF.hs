
{-# LANGUAGE MagicHash #-}

module Smten.Runtime.IntFF (
    IntFF(..), ite_IntFF, applyToIntFF,
  ) where

import GHC.Prim
import GHC.Types
import qualified Data.IntMap as M
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.BoolF
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


-- TODO: the call to ite0 here loses information that 'v' is finite.
-- Maybe we could add a method to ite to explicitly convey when the
-- argument is finite?
applyToIntFF :: (SmtenHS0 a) => (Int# -> a) -> IntFF -> a
applyToIntFF f x =
  case x of
    IntFF i -> f i
    Symbolic_IntFF m ->
      let g [] = unreachable
          g ((I# k,v):xs) = ite0 (finiteF v) (f k) (g xs)
      in g (M.assocs m)
    Unreachable_IntFF -> unreachable
