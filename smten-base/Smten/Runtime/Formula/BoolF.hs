
-- | Representation of SMT boolean formulas which may contain _|_ in subterms.
module Smten.Runtime.Formula.BoolF (
    BoolF, trueF, falseF, boolF, andF, notF, iteF, iteF_, iteF__,
    varF, finiteF,
    unreachableF, isUnreachableF,
    isTrueF, isFalseF, (*.),
    deBoolF,
  ) where

import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.StableNameEq
import qualified Smten.Runtime.Select as S

-- | Representation of a boolean formula x which may contain
-- infinite parts or _|_.
-- We represent the formula as follows:
--  BoolF p a b
--    Where:
--     * x = if p then a else b
--     * p and a are finite
--     * b 
--     * b has not yet finished evaluating to weak head normal form: it
--       might be _|_.
data BoolF = BoolF BoolFF BoolFF BoolF
           | BoolF_Unreachable
     deriving (Show)

deBoolF :: BoolF -> (BoolFF, BoolFF, BoolF)
deBoolF (BoolF p a b) = (p, a, b)
deBoolF BoolF_Unreachable = (Unreachable_BoolFF, Unreachable_BoolFF, BoolF_Unreachable)

-- Construct a finite BoolF of the form:
--   a
-- where a is finite.
finiteF :: BoolFF -> BoolF
finiteF x = BoolF trueFF x BoolF_Unreachable

unreachableF :: BoolF
unreachableF = BoolF_Unreachable

approxF :: BoolF -> BoolF
approxF x = BoolF falseFF Unreachable_BoolFF x

-- Select between two formulas.
-- selectF x y
--   x, y may be infinite.
-- The select call waits for at least one of x or y to reach weak head
-- normal form, then returns WHNF representations for both x and y.
selectF :: BoolF -> BoolF -> (BoolF, BoolF)
selectF x y = S.approximate (approxF x) (approxF y) x y

trueF :: BoolF
trueF = finiteF TrueFF

falseF :: BoolF
falseF = finiteF FalseFF

boolF :: Bool -> BoolF
boolF True = trueF
boolF False = falseF

varF :: FreeID -> BoolF
varF x = finiteF (varFF x)

-- Apply a unary function which is strict and finite.
unaryF :: (BoolFF -> BoolFF) -> BoolF -> BoolF
unaryF f BoolF_Unreachable = BoolF_Unreachable
unaryF f (BoolF p a b) = BoolF p (f a) (unaryF f b)

notF :: BoolF -> BoolF
notF  = unaryF notFF

-- x_ * y_
andF :: BoolF -> BoolF -> BoolF
andF x@(BoolF xp xa xx) y =
  case selectF x y of
    (_, BoolF yp ya yy) ->
      let p = xp * yp
          a = xa * ya
          b = iteF_ xp (unaryF (andFF xa) yy)
                (iteF_ yp (unaryF (andFF ya) xx) (andF xx yy))
      in iteF__ p a b
    -- If y is unreachable, x * y may still be reachable, but in that
    -- case, x must be False, in which case x * y is False.
    -- If x is True, however, then the entire thing must be unreachable.
    (BoolF TrueFF _ _, BoolF_Unreachable) -> BoolF_Unreachable
    (_, BoolF_Unreachable) -> falseF
andF BoolF_Unreachable _ = BoolF_Unreachable

iteF :: BoolF -> BoolF -> BoolF -> BoolF
iteF (BoolF TrueFF p _) x_ y_ = iteF_ p x_ y_
iteF p@(BoolF pp pa pb) x_ y_ =
    case selectF x_ y_ of
      _ | x_ `stableNameEq` y_ -> x_
      (BoolF xp xa xb, BoolF yp ya yb) ->
        let p' = pp `andFF` (iteFF pa xp yp)
            a' = iteFF pa xa ya
            b' = iteF_ pp (iteF_ pa x_ y_) (iteF pb x_ y_)
        in iteF__ p' a' b'
      (BoolF_Unreachable, _) -> y_
      (_, BoolF_Unreachable) -> x_
iteF BoolF_Unreachable _ _ = BoolF_Unreachable

-- iteF with finite predicate
iteF_ :: BoolFF -> BoolF -> BoolF -> BoolF
iteF_ TrueFF x_ _ = x_
iteF_ FalseFF _ y_ = y_
iteF_ p x_ y_
 | x_ `stableNameEq` y_ = x_
 | otherwise = 
    case selectF x_ y_ of
      (BoolF xp xa xx, BoolF yp ya yy) ->
        let p' = iteFF p xp yp
            a' = iteFF p xa ya
            b' = iteF_ p xx yy
        in iteF__ p' a' b'
      (BoolF_Unreachable, _) -> y_
      (_, BoolF_Unreachable) -> x_

-- iteF with finite predicate and True branch
--  x = if p then a else b
--    = p & a | ~p & b
iteF__ :: BoolFF -> BoolFF -> BoolF -> BoolF
iteF__ = BoolF

-- For nicer syntax, we give an instance of Num for BoolF
-- based on boolean arithmetic.
instance Num BoolF where
  fromInteger 0 = falseF
  fromInteger 1 = trueF
  (+) = error "BoolF.+"
  (*) = andF
  negate = notF
  abs = error "BoolF.abs"
  signum = error "BoolF.signum"

-- | Logical AND of a finite formula and a partial formula.
(*.) :: BoolFF -> BoolF -> BoolF
(*.) x y@(BoolF {}) = unaryF (andFF x) y
(*.) TrueFF BoolF_Unreachable = BoolF_Unreachable
(*.) _ BoolF_Unreachable = falseF

-- | Return True if this object is equal to trueF
isTrueF :: BoolF -> Bool
isTrueF (BoolF TrueFF TrueFF _) = True
isTrueF _ = False

-- | Return True if this object is equal to falseF
isFalseF :: BoolF -> Bool
isFalseF (BoolF TrueFF FalseFF _) = True
isFalseF _ = False

isUnreachableF :: BoolF -> Bool
isUnreachableF BoolF_Unreachable = True
isUnreachableF _ = False

