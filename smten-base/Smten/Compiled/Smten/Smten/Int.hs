
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Int (
    Int(..), __I#, __applyToInt,
  ) where

import qualified Prelude as P
import qualified GHC.Types as P
import qualified GHC.Prim as P

import Smten.Runtime.Formula.PartialF
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

data IntFF =
   IntFF P.Int#
 | Ite_IntFF BoolFF IntFF IntFF
 | Unreachable_IntFF

ite_IntFF :: BoolFF -> IntFF -> IntFF -> IntFF 
ite_IntFF TrueFF a _ = a
ite_IntFF FalseFF _ b = b
ite_IntFF Unreachable_BoolFF _ _ = Unreachable_IntFF
ite_IntFF p v@(IntFF a) (IntFF b) | a P.==# b = v
ite_IntFF _ Unreachable_IntFF b = b
ite_IntFF _ a Unreachable_IntFF = a
ite_IntFF p a b = Ite_IntFF p a b

-- TODO: the call to ite0 here looses information that 'p' is finite.
-- Maybe we could add a method to ite to explicitly convey when the
-- argument is finite?
applyToIntFF :: (SmtenHS0 a) => (P.Int# -> a) -> IntFF -> a
applyToIntFF f x =
  case x of
    IntFF i -> f i
    Ite_IntFF p a b -> ite0 (finiteF p) (applyToIntFF f a) (applyToIntFF f b)
    Unreachable_IntFF -> unreachable

type Int = IntF

newtype IntF = IntF (PartialF IntFF)

instance Finite IntFF where
    ite_finite = ite_IntFF
    unreachable_finite = Unreachable_IntFF

__I# :: P.Int# -> Int
__I# v = IntF (finitePF (IntFF v))

ite_IntF :: BoolF -> IntF -> IntF -> IntF
ite_IntF (BoolF p) (IntF a) (IntF b) = IntF (itePF p a b)

unreachable_IntF :: IntF
unreachable_IntF = IntF unreachablePF

instance SymbolicOf P.Int IntF where
    tosym (P.I# x) = __I# x
    symapp f x = __applyToInt (\v -> f (P.I# v)) x

-- TODO: the call to ite0 here looses information that 'p' is finite.
-- Maybe we could add a method to ite to explicitly convey when the
-- argument is finite?
__applyToInt :: (SmtenHS0 a) => (P.Int# -> a) -> Int -> a
__applyToInt f (IntF (PartialF TrueFF (IntFF v) _)) = f v
__applyToInt f (IntF (PartialF p a b_)) = ite0 (finiteF p) (applyToIntFF f a) (__applyToInt f (IntF b_))

instance SmtenHS0 IntF where
    ite0 = ite_IntF
    unreachable0 = unreachable_IntF

instance P.Num IntF where
    fromInteger = tosym P.. (P.fromInteger :: P.Integer -> P.Int)
    (+) = P.error "Smten Int P.Num (+) not supported"
    (*) = P.error "Smten Int P.Num (*) not supported"
    abs = P.error "Smten Int P.Num abs not supported"
    signum = P.error "Smten Int P.Num signum not supported"
