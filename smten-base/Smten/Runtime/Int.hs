

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Implementation of primitive Int# type for Smten.
module Smten.Runtime.Int (
    Int#(..), __isLitInt#,
    (/=#), (==#), (>=#), (<=#), (>#), (<#),
    (+#), (-#), (*#), negateInt#,
    quotInt#, remInt#, quotRemInt#,
    primIntApp, int#,
  ) where

import qualified Prelude as P
import qualified GHC.Types as P
import qualified GHC.Prim as P
import qualified Data.IntMap as M

import Smten.Runtime.Bool
import Smten.Runtime.Formula.PartialF
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula
import Smten.Runtime.SmtenHS

data IntFF =
   IntFF P.Int#
 | Symbolic_IntFF (M.IntMap BoolFF)
 | Unreachable_IntFF

ite_IntFF :: BoolFF -> IntFF -> IntFF -> IntFF 
ite_IntFF TrueFF a _ = a
ite_IntFF FalseFF _ b = b
ite_IntFF Unreachable_BoolFF _ _ = Unreachable_IntFF
ite_IntFF p v@(IntFF a) (IntFF b) | a P.==# b = v
ite_IntFF _ Unreachable_IntFF b = b
ite_IntFF _ a Unreachable_IntFF = a
ite_IntFF p a b =
  let ma = case a of
             IntFF av -> M.singleton (P.I# av) trueFF
             Symbolic_IntFF m -> m
      mb = case b of
             IntFF bv -> M.singleton (P.I# bv) trueFF
             Symbolic_IntFF m -> m
      f _ va vb = P.Just (iteFF p va vb)
      o1 ma = M.map (andFF p) ma
      o2 mb = M.map (andFF (notFF p)) mb
  in Symbolic_IntFF (M.mergeWithKey f o1 o2 ma mb)


-- TODO: the call to ite0 here loses information that 'v' is finite.
-- Maybe we could add a method to ite to explicitly convey when the
-- argument is finite?
applyToIntFF :: (SmtenHS0 a) => (P.Int# -> a) -> IntFF -> a
applyToIntFF f x =
  case x of
    IntFF i -> f i
    Symbolic_IntFF m ->
      let g [] = unreachable
          g ((P.I# k,v):xs) = ite0 (finiteF v) (f k) (g xs)
      in g (M.assocs m)
    Unreachable_IntFF -> unreachable

newtype Int# = Int# (PartialF IntFF)

instance Finite IntFF where
    ite_finite = ite_IntFF
    unreachable_finite = Unreachable_IntFF

ite_IntF :: BoolF -> Int# -> Int# -> Int#
ite_IntF (BoolF p) (Int# a) (Int# b) = Int# (itePF p a b)

unreachable_IntF :: Int#
unreachable_IntF = Int# unreachablePF

-- TODO: the call to ite0 here looses information that 'p' is finite.
-- Maybe we could add a method to ite to explicitly convey when the
-- argument is finite?
primIntApp :: (SmtenHS0 a) => (P.Int# -> a) -> Int# -> a
primIntApp f (Int# (PartialF TrueFF (IntFF v) _)) = f v
primIntApp f (Int# (PartialF p a b_)) = ite0 (finiteF p) (applyToIntFF f a) (primIntApp f (Int# b_))

instance SmtenHS0 Int# where
    ite0 = ite_IntF
    unreachable0 = unreachable_IntF

int# :: P.Int# -> Int#
int# v = Int# (finitePF (IntFF v))

__isLitInt# :: P.Int# -> Int# -> Bool
__isLitInt# v s = int# v ==# s

instance P.Num Int# where
    fromInteger x =
      case P.fromInteger x of
        P.I# v -> int# v

    (+) = P.error "Smten Int P.Num (+) not supported"
    (*) = P.error "Smten Int P.Num (*) not supported"
    abs = P.error "Smten Int P.Num abs not supported"
    signum = P.error "Smten Int P.Num signum not supported"

(/=#) :: Int# -> Int# -> Bool
(/=#) a b = {-# SCC "PRIM_INT_NE" #-}
    primIntApp (\x ->
    primIntApp (\y ->
        if (x P./=# y) then __True else __False)) a b

(==#) :: Int# -> Int# -> Bool
(==#) a b = {-# SCC "PRIM_INT_EQ" #-}
    primIntApp (\x ->
    primIntApp (\y ->
        if (x P.==# y) then __True else __False)) a b

(>=#) :: Int# -> Int# -> Bool
(>=#) a b = {-# SCC "PRIM_INT_GE" #-}
    primIntApp (\x ->
    primIntApp (\y ->
        if (x P.>=# y) then __True else __False)) a b

(<=#) :: Int# -> Int# -> Bool
(<=#) a b = {-# SCC "PRIM_INT_LE" #-}
    primIntApp (\x ->
    primIntApp (\y ->
        if (x P.<=# y) then __True else __False)) a b

(>#) :: Int# -> Int# -> Bool
(>#) a b = {-# SCC "PRIM_INT_GT" #-}
    primIntApp (\x ->
    primIntApp (\y ->
        if (x P.># y) then __True else __False)) a b

(<#) :: Int# -> Int# -> Bool
(<#) a b = {-# SCC "PRIM_INT_LT" #-}
    primIntApp (\x ->
    primIntApp (\y ->
        if (x P.<# y) then __True else __False)) a b

{-# INLINE binint #-}
binint :: (P.Int# -> P.Int# -> P.Int#) -> Int# -> Int# -> Int#
binint f a b = primIntApp (\x -> primIntApp (\y -> int# (f x y))) a b

{-# INLINE unint #-}
unint :: (P.Int# -> P.Int#) -> Int# -> Int#
unint f a = primIntApp (\x -> int# (f x)) a

(+#) :: Int# -> Int# -> Int#
(+#) = {-# SCC "PRIM_INT_ADD" #-} binint (P.+#)

(-#) :: Int# -> Int# -> Int#
(-#) = {-# SCC "PRIM_INT_SUB" #-} binint (P.-#)

(*#) :: Int# -> Int# -> Int#
(*#) = {-# SCC "PRIM_INT_MUL" #-} binint (P.*#)

quotInt# :: Int# -> Int# -> Int#
quotInt# = {-# SCC "PRIM_QUOT_INT" #-} binint P.quotInt#

remInt# :: Int# -> Int# -> Int#
remInt# = {-# SCC "PRIM_REM_INT" #-} binint P.remInt#

quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
quotRemInt# a b = {-# SCC "PRIM_QUOTREM_INT" #-} (# quotInt# a b, remInt# a b #)

negateInt# :: Int# -> Int#
negateInt# = {-# SCC "PRIM_NEGATE_INT" #-} unint P.negateInt#

