
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
-- We represent the formula in one of two ways:
--  Finite - the entire formula is finite and contains no _|_.
--  Partial a b x_ - the formula is partially finite.
--    Where:
--     * Logically this is equivalent to:  a + bx_
--     * Note: bx_ can only be true if b is satisfiable
--     * 'a' and 'b' are finite
--     * 'x_' has not yet finished evaluating to weak head normal form: it
--       might be _|_.
--  By convention in the code that follows, a boolean variable
--  name ending in an underscore refers to a potential _|_ value. A boolean
--  variable name not ending in an underscore refers to a finite value.
data BoolF =
   Finite BoolFF
 | Partial BoolFF BoolFF BoolF
     deriving (Show)

trueF :: BoolF
trueF = Finite trueFF

falseF :: BoolF
falseF = Finite falseFF

varF :: FreeID -> BoolF
varF = Finite . varFF

-- Construct a finite BoolF of the form:
--   a
-- where a is finite.
finiteF :: BoolFF -> BoolF
finiteF = Finite

-- Construct a partially finite BoolF of the form:
--   a + bx_
-- where a, b are finite, x_ is potentially _|_
-- This is lazy in x_.
partialF :: BoolFF -> BoolFF -> BoolF -> BoolF
partialF TrueFF _ _ = trueF
partialF a FalseFF _ = Finite a
partialF FalseFF TrueFF x = x
partialF a b c = Partial a b c

-- Construct a partially finite BoolF of the form:
--   a + bx_ + cy_ + dx_y_
-- where a, b, c, d are finite, x_ and y_ are potentially _|_
-- This is lazy in both x_ and y_
-- Note: a + bx_ + cy_ + dx_y_ = a + (b+c+d)(bx_ + cy_ + x_y_)
partial2F :: BoolFF -> BoolFF -> BoolFF -> BoolFF -> BoolF -> BoolF -> BoolF
partial2F TrueFF _ _ _ _ _ = trueF
partial2F a FalseFF c FalseFF _ y_ = partialF a c y_
partial2F a b FalseFF FalseFF x_ _ = partialF a b x_
partial2F a b c d x_ y_ = partialF a (b + c + d) (wait2F trueFF b c trueFF x_ y_)

-- Wait for more information about a BoolF of the form:
--   a + bx_ + cy_ + dx_y_
-- where a, b, c, d are finite, x_ and y_ are potentially _|_
-- This is strict in one of x_ or y_ (which ever finishes first)
wait2F :: BoolFF -> BoolFF -> BoolFF -> BoolFF -> BoolF -> BoolF -> BoolF
wait2F a b c d x_ y_ =
  case select x_ y_ of
    Both (Finite x) (Finite y) -> finiteF (a + b*x + c*y + d*x*y)
    Both (Finite x) (Partial ya yb yc_) -> 
      let c_dx = c + d*x
      in partialF (a + b*x + c_dx*ya) (c_dx*yb) yc_
    Both (Partial xa xb xc_) (Finite y) ->
      let b_dy = b + d*y
      in partialF (a + c*y + b_dy*xa) (b_dy*xb) xc_
    Both (Partial xa xb xc_) (Partial ya yb yc_) ->
      let a' = a + b*xa + c*ya + d*xa*ya
          b' = xb*(b + ya)
          c' = yb*(c + xa)
          d' = xb*yb
      in partial2F a' b' c' d' xc_ yc_
    Left (Finite x) -> partialF (a + b*x) (c + d*x) y_
    Left (Partial xa xb xc_) ->
      let a' = a + b*xa
          b' = b * xb
          c' = c + d*xa
          d' = d * xb
      in partial2F a' b' c' d' xc_ y_
    Right (Finite y) -> partialF (a + c*y) (b + d*y) x_
    Right (Partial ya yb yc_) ->
      let a' = a + c*ya
          b' = b + d*ya
          c' = c * yb
          d' = d * yb
      in partial2F a' b' c' d' x_ yc_
    

-- Notes
--  Finite:     -a = -a
--  Partial:    -(a + bx_)
--            = (-a)(-(bx_))
--            = (-a)(-b + (-x_))
--            = (-a)(-b) + (-a)(-x_)
notF :: BoolF -> BoolF
notF (Finite a) = Finite (-a)
notF (Partial a b x_) =
  let nota = (-a)
  in partialF (nota * (-b)) nota (notF x_)

-- x_ * y_
-- Note: x_ * y_ = 0 + 0*x_ + 0*y_ + 1*x_*y_
andF :: BoolF -> BoolF -> BoolF
andF = wait2F falseFF falseFF falseFF trueFF

-- x_ + y_
-- Note: x_ + y_ = 0 + 1*x_ + 1*y_ + 0*x_*y_
orF :: BoolF -> BoolF -> BoolF
orF = wait2F falseFF trueFF trueFF falseFF

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
