
module Smten.Runtime.Formula.IntegerF (
    IntegerF, integerF, var_IntegerF, ite_IntegerF,
    eq_IntegerF, leq_IntegerF, add_IntegerF, sub_IntegerF,
    deIntegerF, finite_IntegerF, unreachable_IntegerF,
  ) where

import Smten.Runtime.FreeID
import Smten.Runtime.Formula.BoolF
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.PartialF

-- We need to use a newtype instead of a type synonym to get around annoying
-- issues with overlapping instances of SmtenHS0 for IntegerF
newtype IntegerF = IntegerF (PartialF IntegerFF)

instance Finite IntegerFF where
    ite_finite = ite_IntegerFF
    unreachable_finite = Unreachable_IntegerFF

integerF :: Integer -> IntegerF
integerF x = IntegerF $ finitePF (integerFF x)

var_IntegerF :: FreeID -> IntegerF
var_IntegerF x = IntegerF $ finitePF (var_IntegerFF x)

eq_IntegerF :: IntegerF -> IntegerF -> BoolF
eq_IntegerF (IntegerF a) (IntegerF b) = BoolF $ binaryPF eq_IntegerFF a b

leq_IntegerF :: IntegerF -> IntegerF -> BoolF
leq_IntegerF (IntegerF a) (IntegerF b) = BoolF $ binaryPF leq_IntegerFF a b

add_IntegerF :: IntegerF -> IntegerF -> IntegerF
add_IntegerF (IntegerF a) (IntegerF b) = IntegerF $ binaryPF add_IntegerFF a b

sub_IntegerF :: IntegerF -> IntegerF -> IntegerF
sub_IntegerF (IntegerF a) (IntegerF b) = IntegerF $ binaryPF sub_IntegerFF a b

ite_IntegerF :: BoolF -> IntegerF -> IntegerF -> IntegerF
ite_IntegerF (BoolF p) (IntegerF a) (IntegerF b) = IntegerF $ itePF p a b

-- | Decompose an IntegerF into finite and possibly not finite parts.
-- deIntegerF x = (p, a, b_)
--   where x = if p then a else b_
--         p, a finite
--         b_ possibly not finite.
deIntegerF :: IntegerF -> (BoolFF, IntegerFF, IntegerF)
deIntegerF (IntegerF (PartialF p a b_)) = (p, a, IntegerF b_)
    
finite_IntegerF :: IntegerFF -> IntegerF
finite_IntegerF x = IntegerF (finitePF x)

unreachable_IntegerF :: IntegerF
unreachable_IntegerF = IntegerF unreachablePF

