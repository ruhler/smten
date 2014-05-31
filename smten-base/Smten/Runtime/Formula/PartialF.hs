
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Runtime.Formula.PartialF (
    PartialF(..), Finite(..), 
    finitePF, itePF, itePF_, andPF,
    unaryPF, binaryPF, unreachablePF,
  )  where

import Smten.Runtime.Formula.Finite
import Smten.Runtime.StableNameEq
import qualified Smten.Runtime.Select as S

-- | Representation of a formula which may contain infinite parts or _|_
-- We represent the formula as follows:
--  PartialF p a b_
--    Where:
--     * Logically this is equivalent to:  if p then a else b_
--     * 'p' and 'a' are finite
--     * 'b_' has not yet finished evaluating to weak head normal form: it
--       might be _|_.
data PartialF a = PartialF BoolFF a (PartialF a)
                 | Unreachable_PartialF
    deriving (Show)

class Finite a where
    ite_finite :: BoolFF -> a -> a -> a
    unreachable_finite :: a

instance Finite BoolFF where
    ite_finite = iteFF
    unreachable_finite = Unreachable_BoolFF

instance (Finite b) => Finite (a -> b) where
    ite_finite p a b = \x -> ite_finite p (a x) (b x)
    unreachable_finite = \x -> unreachable_finite

finitePF :: a -> PartialF a
finitePF x = PartialF trueFF x unreachablePF

unreachablePF :: PartialF a
unreachablePF = Unreachable_PartialF

{-# INLINEABLE approxPF #-}
approxPF :: (Finite a) => PartialF a -> PartialF a
approxPF x = PartialF falseFF unreachable_finite x

-- Select between two formulas.
-- pselectF x_ y_
--   x_, y_ may be infinite.
-- The select call waits for at least one of x_ or y_ to reach weak head
-- normal form, then returns WHNF representations for both x_ and y_.
{-# INLINEABLE selectPF #-}
selectPF :: (Finite a, Finite b) => PartialF a -> PartialF b -> (PartialF a, PartialF b)
selectPF x_ y_ = S.approximate (approxPF x_) (approxPF y_) x_ y_

-- Apply a unary function which is strict and finite
{-# INLINEABLE unaryPF #-}
unaryPF :: (a -> b) -> PartialF a -> PartialF b
unaryPF f Unreachable_PartialF = unreachablePF
unaryPF f (PartialF p a b) = PartialF p (f a) (unaryPF f b)

-- Apply a binary function which is strict and finite
{-# INLINEABLE binaryPF #-}
binaryPF :: (Finite a, Finite b, Finite c) => (a -> b -> c) -> PartialF a -> PartialF b -> PartialF c
binaryPF f x_ y_ = 
  case (x_, y_) of
    (PartialF xp xa xb_, PartialF yp ya yb_) ->
      let p = xp * yp
          a = f xa ya
          c_ = itePF_ xp (unaryPF (f xa) yb_)
                    (itePF_ yp (unaryPF (flip f ya) xb_) (binaryPF f xb_ yb_))
      in itePF__ p a c_
    (Unreachable_PartialF, _) -> unreachablePF
    (_, Unreachable_PartialF) -> unreachablePF

-- x_ * y_
-- This is strict in x_:
--   _|_ * False   is _|_, not False
-- This corresponds to the fact that:
--  search (_|_ >> mzero)   is _|_, not Nothing
andPF :: PartialF BoolFF -> PartialF BoolFF -> PartialF BoolFF
andPF x@(PartialF xp xa xx) y =
  case selectPF x y of
    (_, PartialF yp ya yy) ->
      let p = xp * yp
          a = xa * ya
          b = itePF_ xp (unaryPF (andFF xa) yy)
                (itePF_ yp (unaryPF (andFF ya) xx) (andPF xx yy))
      in itePF__ p a b
    -- If y is unreachable, x && y may still be reachable, but in that
    -- case, x must be False, in which case x && y is False.
    -- If x is True, however, then the entire thing must be unreachable.
    (PartialF TrueFF _ _, Unreachable_PartialF) -> unreachablePF
    (_, Unreachable_PartialF) -> finitePF falseFF
andPF Unreachable_PartialF _ = unreachablePF

-- This is strict in the first argument.
-- Corresponding to the fact that:
--  if _|_ then x else x    is  _|_, not x
{-# INLINEABLE itePF #-}
itePF :: (Finite a) => PartialF BoolFF -> PartialF a -> PartialF a -> PartialF a
itePF (PartialF TrueFF p _) x_ y_ = itePF_ p x_ y_
itePF (PartialF pp pa pb) x_ y_ =
     case selectPF x_ y_ of
       _ | x_ `stableNameEq` y_ -> x_
       (PartialF xp xa xb_, PartialF yp ya yb_) ->
          let p = pp `andFF` (iteFF pa xp yp)
              a = ite_finite pa xa ya
              b_ = itePF_ pp (itePF_ pa x_ y_) (itePF pb x_ y_)
          in itePF__ p a b_
       (Unreachable_PartialF, _) -> y_
       (_, Unreachable_PartialF) -> x_
itePF Unreachable_PartialF _ _ = Unreachable_PartialF

{-# INLINEABLE itePF_ #-}
itePF_ :: (Finite a) => BoolFF -> PartialF a -> PartialF a -> PartialF a
itePF_ TrueFF x_ _ = x_
itePF_ FalseFF _ y_ = y_
itePF_ p x_ y_
 | x_ `stableNameEq` y_ = x_
 | otherwise = 
    case selectPF x_ y_ of
      (PartialF xp xa xx, PartialF yp ya yy) ->
        let p' = iteFF p xp yp
            a' = ite_finite p xa ya
            b' = itePF_ p xx yy
        in itePF__ p' a' b'
      (Unreachable_PartialF, _) -> y_
      (_, Unreachable_PartialF) -> x_

itePF__ :: BoolFF -> a -> PartialF a -> PartialF a
itePF__ = PartialF

