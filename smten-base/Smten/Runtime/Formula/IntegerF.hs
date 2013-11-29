
module Smten.Runtime.Formula.IntegerF (
    IntegerF, integerF, var_IntegerF, ite_IntegerF,
    eq_IntegerF, leq_IntegerF, add_IntegerF, sub_IntegerF,
    parts_IntegerF, finite_IntegerF,
  ) where

import Smten.Runtime.FreeID
import Smten.Runtime.Formula.BoolF
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.PartialF

-- We need to use a newtype instead of a type synonym to get around annoying
-- issues with overlapping instances of SmtenHS0 for IntegerF
newtype IntegerF = IntegerF (PartialF IntegerFF)

instance IsFinite IntegerFF where
    finite_iteFF = iiteFF

integerF :: Integer -> IntegerF
integerF x = IntegerF $ pfiniteF (integerFF x)

var_IntegerF :: FreeID -> IntegerF
var_IntegerF x = IntegerF $ pfiniteF (ivarFF x)

eq_IntegerF :: IntegerF -> IntegerF -> BoolF
eq_IntegerF (IntegerF a) (IntegerF b) = binarypF ieqFF a b

leq_IntegerF :: IntegerF -> IntegerF -> BoolF
leq_IntegerF (IntegerF a) (IntegerF b) = binarypF ileqFF a b

add_IntegerF :: IntegerF -> IntegerF -> IntegerF
add_IntegerF (IntegerF a) (IntegerF b) = IntegerF $ binaryoF iaddFF a b

sub_IntegerF :: IntegerF -> IntegerF -> IntegerF
sub_IntegerF (IntegerF a) (IntegerF b) = IntegerF $ binaryoF isubFF a b

ite_IntegerF :: BoolF -> IntegerF -> IntegerF -> IntegerF
ite_IntegerF p (IntegerF a) (IntegerF b) = IntegerF $ ite_PartialF p a b

-- | Decompose an IntegerF into finite and possibly not finite parts.
-- parts_IntegerF x = (p, a, b_)
--   where x = if p then a else b_
--         p, a finite
--         b_ possibly not finite.
parts_IntegerF :: IntegerF -> (BoolFF, IntegerFF, IntegerF)
parts_IntegerF (IntegerF (PartialF p a b_)) = (p, a, IntegerF b_)

finite_IntegerF :: IntegerFF -> IntegerF
finite_IntegerF x = IntegerF (pfiniteF x)

