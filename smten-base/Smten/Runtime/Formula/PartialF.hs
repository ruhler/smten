
module Smten.Runtime.Formula.PartialF (
    PartialF(..), IsFinite(..), pfiniteF, ite_PartialF,
    unarypF, binarypF, unaryoF, binaryoF,
  )  where

import Smten.Runtime.Formula.BoolF
import Smten.Runtime.Formula.Finite
import qualified Smten.Runtime.Select as S

-- | Representation of a formula which may contain infinite parts or _|_
-- We represent the formula as follows:
--  PartialF p a b_
--    Where:
--     * Logically this is equivalent to:  if p then a else b_
--     * 'p' and 'a' are finite
--     * 'b_' has not yet finished evaluating to weak head normal form: it
--       might be _|_.
-- We share this representation for both integers and bit vectors.
data PartialF a = PartialF BoolFF a (PartialF a)
                | PartialF_Unreachable

class IsFinite a where
    finite_iteFF :: BoolFF -> a -> a -> a
    finite_unreachable :: a

-- Select between two formulas.
-- pselectF x_ y_
--   x_, y_ may be infinite.
-- The select call waits for at least one of x_ or y_ to reach weak head
-- normal form, then returns WHNF representations for both x_ and y_.
pselectF :: (IsFinite a) => PartialF a -> PartialF a -> (PartialF a, PartialF a)
pselectF x_ y_ = 
  case S.select x_ y_ of
    S.Both x y -> (x, y)
    S.Left x -> (x, PartialF falseFF finite_unreachable y_)
    S.Right y -> (PartialF falseFF finite_unreachable x_, y)

pfiniteF :: a -> PartialF a
pfiniteF x = PartialF trueFF x PartialF_Unreachable

-- partial unary predicate
unarypF :: (a -> BoolFF) -> PartialF a -> BoolF
unarypF f (PartialF p a b_) = partialF (p * f a) (notFF p) (unarypF f b_)
unarypF _ (PartialF_Unreachable) = BoolF_Unreachable

-- partial binary predicate
binarypF :: (IsFinite a) => (a -> a -> BoolFF) -> PartialF a -> PartialF a -> BoolF
binarypF f x_ y_ = 
  case pselectF x_ y_ of
    (PartialF xp xa xb_, PartialF yp ya yb_) ->
      let p = xp * yp
          a = p * (f xa ya)
          b = notFF p
          c_ = iteF (finiteF xp) (unarypF (f xa) yb_)
                    (iteF (finiteF yp) (unarypF (f ya) xb_) (binarypF f xb_ yb_))
      in partialF a b c_
    (PartialF_Unreachable, _) -> BoolF_Unreachable
    (_, PartialF_Unreachable) -> BoolF_Unreachable

-- parital unary operator
unaryoF :: (a -> a) -> PartialF a -> PartialF a
unaryoF f (PartialF p a b_) = PartialF p (f a) (unaryoF f b_)
unaryoF _ (PartialF_Unreachable) = PartialF_Unreachable

-- partial binary operator
binaryoF :: (IsFinite a) => (a -> a -> a) -> PartialF a -> PartialF a -> PartialF a
binaryoF f x_ y_ =
  case pselectF x_ y_ of
    (PartialF xp xa xb_, PartialF yp ya yb_) ->
      let p = xp * yp
          a = f xa ya
          b_ = ite_PartialF (finiteF xp) (unaryoF (f xa) yb_)
                     (ite_PartialF (finiteF yp) (unaryoF (f ya) xb_) (binaryoF f xb_ yb_))
      in PartialF p a b_
    (PartialF_Unreachable, _) -> PartialF_Unreachable
    (_, PartialF_Unreachable) -> PartialF_Unreachable

ite_PartialF :: (IsFinite a) => BoolF -> PartialF a -> PartialF a -> PartialF a
ite_PartialF p a b
  | isTrueF p = a
  | isFalseF p = b
ite_PartialF (BoolF pa pb pc_) x_ y_ = 
  case pselectF x_ y_ of
    (PartialF xp xa xb_, PartialF yp ya yb_) ->
       let p = iteFF pa xp (notFF pb * yp)
           a = finite_iteFF pa xa ya
           b_ = ite_PartialF (finiteF pa) xb_ (ite_PartialF (finiteF pb) (ite_PartialF pc_ x_ y_) yb_)
       in PartialF p a b_
    (PartialF_Unreachable, _) -> y_
    (_, PartialF_Unreachable) -> x_
ite_PartialF (BoolF_Unreachable) _ _ = PartialF_Unreachable

