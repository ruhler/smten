
-- | Representation of SMT boolean formulas which may contain _|_ in subterms.
module Smten.Runtime.Formula.BoolF (
    BoolF(..), trueF, falseF, boolF, andF, notF, iteF, varF, finiteF, partialF,
    isTrueF, isFalseF, (*.),
  ) where

import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.StableNameEq
import qualified Smten.Runtime.Select as S

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

-- Select between two formulas.
-- selectF x_ y_
--   x_, y_ may be infinite.
-- The select call waits for at least one of x_ or y_ to reach weak head
-- normal form, then returns WHNF representations for both x_ and y_.
selectF :: BoolF -> BoolF -> (BoolF, BoolF)
selectF x_ y_ = 
  case S.select x_ y_ of
    S.Both x y -> (x, y)
    S.Left x -> (x, BoolF falseFF trueFF y_)
    S.Right y -> (BoolF falseFF trueFF x_, y)

trueF :: BoolF
trueF = finiteF trueFF

falseF :: BoolF
falseF = finiteF falseFF

boolF :: Bool -> BoolF
boolF True = trueF
boolF False = falseF

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
andF :: BoolF -> BoolF -> BoolF
andF x_@(BoolF xa xb xc_) y_ =
  case selectF x_ y_ of
    (_, BoolF ya yb yc_) ->
      let a = xa * ya
          xayb = xa * yb
          yaxb = ya * xb
          b = xayb + yaxb + xb * yb
          c_ = xayb *. yc_ + yaxb *. xc_ + xc_ * yc_
      in partialF a b c_

-- TODO: remove this once ite is implemented properly
-- x_ + y_
orF :: BoolF -> BoolF -> BoolF
orF x_ y_ =
  case selectF x_ y_ of
    (BoolF xa xb xc_, BoolF ya yb yc_) ->
      let a = xa + ya
          b = xb + yb
          c_ = xb *. xc_ + yb *. yc_
      in partialF a b c_

iteF :: BoolF -> BoolF -> BoolF -> BoolF
iteF p@(BoolF pa pb pc_) x_ y_
 | isTrueF p = x_
 | isFalseF p = y_
 | x_ `stableNameEq` y_ = x_
 | otherwise = (p `andF` x_) `orF` (notF p `andF` y_)
--    case selectF x_ y_ of
--      (BoolF xa xb xc_, BoolF ya yb yc_) ->
--        let a = iteFF pa xa (notFF pb * ya)
--            b = pb * (xa + xb) + iteFF pa xb (ya + yb)
--            c_ = xb*.(xc_*p) + (-pa)*.((-pc_)*y_) + iteF (finiteF pb) (xa*.pc_) (((-pa)*yb)*.yc_)
--        in partialF a b c_

-- For nicer syntax, we give an instance of Num for BoolF
-- based on boolean arithmetic.
instance Num BoolF where
  fromInteger 0 = falseF
  fromInteger 1 = trueF
  (+) = orF
  (*) = andF
  negate = notF
  abs = error "BoolF.abs"
  signum = error "BoolF.signum"

-- | Logical AND of a finite formula and a partial formula.
(*.) :: BoolFF -> BoolF -> BoolF
(*.) x (BoolF a b c_) = BoolF (x*a) (x*b) c_

-- | Return True if this object is equal to trueF
isTrueF :: BoolF -> Bool
isTrueF (BoolF TrueFF FalseFF _) = True
isTrueF _ = False

-- | Return True if this object is equal to falseF
isFalseF :: BoolF -> Bool
isFalseF (BoolF FalseFF FalseFF _) = True
isFalseF _ = False

