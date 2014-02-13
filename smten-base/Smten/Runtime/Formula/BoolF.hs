
-- | Representation of SMT boolean formulas which may contain _|_ in subterms.
module Smten.Runtime.Formula.BoolF (
    BoolF(BoolF), trueF, falseF, boolF, andF, notF, iteF, iteF_, iteF__,
    varF, finiteF,
    unreachableF, isUnreachableF,
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
           | BoolF_Unreachable
     deriving (Show)

-- Construct a finite BoolF of the form:
--   a
-- where a is finite.
finiteF :: BoolFF -> BoolF
finiteF x = BoolF x falseFF BoolF_Unreachable

-- Construct a partially finite BoolF of the form:
--   a + bx_
-- where a, b are finite, x_ is potentially _|_
-- This is lazy in x_.
partialF :: BoolFF -> BoolFF -> BoolF -> BoolF
partialF TrueFF _ _ = trueF
partialF FalseFF FalseFF _ = falseF
partialF FalseFF TrueFF x_ = x_
partialF a b x_ = BoolF a b x_

unreachableF :: BoolF
unreachableF = BoolF_Unreachable

-- Select between two formulas.
-- selectF x_ y_
--   x_, y_ may be infinite.
-- The select call waits for at least one of x_ or y_ to reach weak head
-- normal form, then returns WHNF representations for both x_ and y_.
selectF :: BoolF -> BoolF -> (BoolF, BoolF)
selectF x_ y_ = S.approximate (BoolF falseFF trueFF x_) (BoolF falseFF trueFF y_) x_ y_

trueF :: BoolF
trueF = finiteF TrueFF

falseF :: BoolF
falseF = finiteF FalseFF

boolF :: Bool -> BoolF
boolF True = trueF
boolF False = falseF

varF :: FreeID -> BoolF
varF x = finiteF (varFF x)

-- Notes
--  Partial:    ~(a + bx_)
--            = (~a)(~(bx_))
--            = (~a)(~b + (~x_))
--            = (~a)(~b) + (~a)(~x_)
--            = ~(a+b) + (~a)(~x_)
notF :: BoolF -> BoolF
notF (BoolF a b x_) = partialF (notFF (a + b)) (notFF a) (notF x_)
notF BoolF_Unreachable = BoolF_Unreachable

-- x_ * y_
andF :: BoolF -> BoolF -> BoolF
andF x_@(BoolF xa xb xc_) y_ =
  case selectF x_ y_ of
    (_, BoolF ya yb yc_) ->
      let a = xa * ya
          b = (xa + xb) * (ya + yb)
          c_ = case selectF xc_ yc_ of
                 (BoolF xca xcb xcc_, BoolF yca ycb ycc_) ->
                    let x_' = partialF (xa + xb*xca) (xb*xcb) xcc_
                        y_' = partialF (ya + yb*yca) (yb*ycb) ycc_
                    in andF x_' y_'
                 (BoolF TrueFF _ _, BoolF_Unreachable) -> BoolF_Unreachable
                 (_, BoolF_Unreachable) -> falseF
                 (BoolF_Unreachable, _) -> BoolF_Unreachable
      in partialF a b c_

    -- If y is unreachable, x * y may still be reachable, but in that
    -- case, x must be False, in which case x * y is False.
    -- If x is True, however, then the entire thing must be unreachable.
    (BoolF TrueFF _ _, BoolF_Unreachable) -> BoolF_Unreachable
    (_, BoolF_Unreachable) -> falseF
andF BoolF_Unreachable _ = BoolF_Unreachable

iteF :: BoolF -> BoolF -> BoolF -> BoolF
iteF (BoolF pa FalseFF _) x_ y_ = iteF_ pa x_ y_
iteF p@(BoolF pa pb pc_) x_ y_ =
    case selectF x_ y_ of
      _ | x_ `stableNameEq` y_ -> x_
      (BoolF xa xb xc_, BoolF ya yb yc_) ->
        let a = iteFF pa xa (notFF pb * ya)
            b = iteFF pa xb (pb + ya + yb)
            c_ = iteF_ pa xc_ (iteF_ pb (iteF pc_ x_ y_) yc_)
        in partialF a b c_
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
      (BoolF xa xb xc_, BoolF ya yb yc_) ->
        let a = iteFF p xa ya
            b = iteFF p xb yb
            c_ = iteF_ p xc_ yc_
        in partialF a b c_
      (BoolF_Unreachable, _) -> y_
      (_, BoolF_Unreachable) -> x_

-- iteF with finite predicate and True branch
--  x = if p then a else b
--    = p & a | ~p & b
iteF__ :: BoolFF -> BoolFF -> BoolF -> BoolF
iteF__ p a b = partialF (p `andFF` a) (notFF p) b

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
(*.) x (BoolF a b c_) = BoolF (x*a) (x*b) c_
(*.) TrueFF BoolF_Unreachable = BoolF_Unreachable
(*.) _ BoolF_Unreachable = falseF

-- | Return True if this object is equal to trueF
isTrueF :: BoolF -> Bool
isTrueF (BoolF TrueFF _ _) = True
isTrueF _ = False

-- | Return True if this object is equal to falseF
isFalseF :: BoolF -> Bool
isFalseF (BoolF FalseFF FalseFF _) = True
isFalseF _ = False

isUnreachableF :: BoolF -> Bool
isUnreachableF BoolF_Unreachable = True
isUnreachableF _ = False

