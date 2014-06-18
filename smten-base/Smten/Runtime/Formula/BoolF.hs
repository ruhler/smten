
-- | Representation of SMT boolean formulas which may contain _|_ in subterms.
module Smten.Runtime.Formula.BoolF (
    BoolF(..), trueF, falseF, boolF, andF, andF_, notF, iteF,
    varF, finiteF,
    unreachableF,
    isTrueF, isFalseF, iteS,
    deBoolF,
  ) where

import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.PartialF

newtype BoolF = BoolF (PartialF BoolFF)
     deriving (Show)

deBoolF :: BoolF -> (BoolFF, BoolFF, BoolF)
deBoolF (BoolF (PartialF p a b)) = (p, a, BoolF b)

-- Construct a finite BoolF of the form:
--   a
-- where a is finite.
finiteF :: BoolFF -> BoolF
finiteF x = BoolF (finitePF x)

unreachableF :: BoolF
unreachableF = BoolF unreachablePF

trueF :: BoolF
trueF = finiteF TrueFF

falseF :: BoolF
falseF = finiteF FalseFF

boolF :: Bool -> BoolF
boolF True = trueF
boolF False = falseF

varF :: FreeID -> BoolF
varF x = finiteF (varFF x)

notF :: BoolF -> BoolF
notF (BoolF x) = BoolF $ unaryPF notFF x

andF :: BoolF -> BoolF -> BoolF
andF (BoolF x) (BoolF y) = BoolF $ andPF x y

andF_ :: BoolFF -> BoolF -> BoolF
andF_ x (BoolF y) = BoolF $ unaryPF (andFF x) y

iteF :: BoolF -> BoolF -> BoolF -> BoolF
iteF (BoolF p) (BoolF x) (BoolF y) = BoolF $ itePF p x y

-- | Return True if this object is equal to trueF
isTrueF :: BoolF -> Bool
isTrueF (BoolF (PartialF TrueFF TrueFF _)) = True
isTrueF _ = False

-- | Return True if this object is equal to falseF
isFalseF :: BoolF -> Bool
isFalseF (BoolF (PartialF TrueFF FalseFF _)) = True
isFalseF _ = False

-- iteS p a b s
--  - if p is Concretely True, then a
--  - if p is Concretely False, then b
--  - if p is Symbolic, then s
iteS :: BoolF -> a -> a -> a -> a
iteS (BoolF (PartialF TrueFF TrueFF _)) a b s = a
iteS (BoolF (PartialF TrueFF FalseFF _)) a b s = b
iteS _ a b s = s


