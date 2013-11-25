
-- | Implementation of SMT formulas which separate finite from potentially
-- infinite results.
module Smten.Runtime.Formula (
    BoolF(..), trueF, falseF, andF, orF, notF, iteF, varF,
    --IntegerF(..), integerF, iaddF, isubFF, iiteF, ivarF, ieqF, ileqF,
  )
  where

import Prelude hiding (Either(..))
import Smten.Runtime.Select
import Smten.Runtime.FiniteFormula

-- | Representation of a boolean formula which may contain infinite parts or _|_
-- We represent the formula as follows:
--  BoolF a b x_
--    Where:
--     * Logically this is equivalent to:  a + b*x_
--     * Note: b*x_ can only be true if b is satisfiable
--       That is, b is an approximation of b*x_
--     * 'a' and 'b' are finite
--     * 'x_' has not yet finished evaluating to weak head normal form: it
--       might be _|_.
--  By convention in the code that follows, a boolean variable
--  name ending in an underscore refers to a potential _|_ value. A boolean
--  variable name not ending in an underscore refers to a finite value.
data BoolF = BoolF BoolFF BoolFF BoolF
     deriving (Show)

-- Construct a finite BoolF of the form:
--   a
-- where a is finite.
finiteF :: BoolFF -> BoolF
finiteF x = BoolF x falseFF (error "finiteF._|_")

-- Construct a partially finite BoolF of the form:
--   a + bx_
-- where a, b are finite, x_ is potentially _|_
-- This is lazy in x_.
partialF :: BoolFF -> BoolFF -> BoolF -> BoolF
partialF = BoolF

-- Construct a partially finite BoolF of the form:
--   a + bx_ + cy_ + dx_y_
-- where a, b, c, d are finite, x_ and y_ are potentially _|_
-- This is lazy in both x_ and y_
-- Note: a + bx_ + cy_ + dx_y_ = a + (b+c+d)(bx_ + cy_ + x_y_)
partial2F :: BoolFF -> BoolFF -> BoolFF -> BoolFF -> BoolF -> BoolF -> BoolF
partial2F TrueFF _ _ _ _ _ = trueF
partial2F a FalseFF c FalseFF _ y_ = partialF a c y_
partial2F a b FalseFF FalseFF x_ _ = partialF a b x_
partial2F a b c d x_ y_ = partialF a (b + c + d) (wait2F b c trueFF x_ y_)

-- Select between two formulas.
-- selectF x_ y_
--   x_, y_ may be infinite.
-- The select call waits for at least one of x_ or y_ to reach weak head
-- normal form, then returns WHNF representations for both x_ and y_.
selectF :: BoolF -> BoolF -> (BoolF, BoolF)
selectF x_ y_ = 
  case select x_ y_ of
    Both x y -> (x, y)
    Left x -> (x, BoolF falseFF trueFF y_)
    Right y -> (y, BoolF falseFF trueFF x_)

-- Wait for more information about a BoolF of the form:
--   bx_ + cy_ + dx_y_
-- where b, c, d are finite, x_ and y_ are potentially _|_
-- This is strict in one of x_ or y_ (which ever finishes first)
wait2F :: BoolFF -> BoolFF -> BoolFF -> BoolF -> BoolF -> BoolF
wait2F b c d x_ y_ =
  case selectF x_ y_ of
    (BoolF xa xb xc_, BoolF ya yb yc_) ->
      let a' = b*xa + c*ya + d*xa*ya
          b' = xb*(b + ya)
          c' = yb*(c + xa)
          d' = xb*yb
      in partial2F a' b' c' d' xc_ yc_

trueF :: BoolF
trueF = finiteF trueFF

falseF :: BoolF
falseF = finiteF falseFF

varF :: FreeID -> BoolF
varF = finiteF . varFF

-- Notes
--  Partial:    ~(a + bx_)
--            = (~a)(~(bx_))
--            = (~a)(~b + (~x_))
--            = (~a)(~b) + (~a)(~x_)
notF :: BoolF -> BoolF
notF (BoolF a b x_) =
  let nota = (-a)
  in partialF (nota * (-b)) nota (notF x_)

-- x_ * y_
-- Note: x_ * y_ = 0 + 0*x_ + 0*y_ + 1*x_*y_
andF :: BoolF -> BoolF -> BoolF
andF = wait2F falseFF falseFF trueFF

-- x_ + y_
-- Note: x_ + y_ = 0 + 1*x_ + 1*y_ + 0*x_*y_
orF :: BoolF -> BoolF -> BoolF
orF = wait2F trueFF trueFF falseFF

iteF :: BoolF -> BoolF -> BoolF -> BoolF
iteF p a b = (p `andF` a) `orF` (notF p `andF` b)


---- | Representation of an integer formula which may contain infinite parts or _|_
---- We represent the formula in one of two ways:
----  IFinite - the entire formula is finite and contains no _|_.
----  IPartial p a x_ - the formula is partially finite.
----    Where:
----      * Logically this is equivalent to: if p then a else x_
----      * 'p' and 'a' are finite
----      * 'x_' has not yet finished evaluating to weak head normal form: it
----        might be _|_.
--data IntegerF =
--   IFinite IntegerFF
-- | IPartial BoolFF IntegerFF IntegerF
--
--integerF :: Integer -> IntegerF
--integerF x = IFinite (integerFF x)
--
--iaddF :: IntegerF -> IntegerF -> IntegerF
--iaddF (IFinite x) (IFinite y) = IFinite (x+y)
--iaddF x@(IFinite xf) (IPartial p a b_) = IPartial p (xf + a) (iaddF x b_)
--iaddF (IPartial p a b_) y@(IFinite yf) = IPartial p (a + yf) (iaddF b_ yf)
--iaddF (IPartial xp xa xb_) (IPartial yp ya yb_) =
--  let rest = case select xb_ yb_ of
--                Both (IFinite xb) (IFinite yb) -> IFinite (iiteFF xp (xa + yb) (iiteFF yp (xb+ya) (xb+yb)))
--                Left (IFinite xb) -> IPartial yp (xb + ya) (iaddF (IFinite $ iiteFF xp xa xb) yb_)
--                Right (IFinite yb) -> IPartial xp (xa + yb) (iaddF xb_ (IFinite $ iiteFF yp ya yb))
--  in IPartial (xp `andFF` yp) (xa + ya) rest
--
--isubFF
--
--iiteF :: BoolF -> IntegerF -> IntegerF
--iiteF (Finite p) a_ b_ =
--  case select a_ b_ of
--    Both (IFinite a) (IFinite b) -> IFinite (iteFF p a b)
--    Left (IFinite a) -> IPartial p a b_
--    Right (IFinite b) -> IPartial (notFF p) b a_
--iiteF (Partial a b x_) t_ f_ =
--  case select t_ f_ of
--
--ivarF :: FreeID -> IntegerF
--ivarF v = IFinite (ivarFF v)
--
--ieqF :: IntegerF -> IntegerF -> BoolF
--ieqF (IFinite x) (IFinite y) = finiteF (ieqFF x y)
--ieqF (IFinite x) (IPartial p a b_) = 
--
--ileqF
--
