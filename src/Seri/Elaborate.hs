
module Seri.Elaborate (
    elaborate
    ) where

import Control.Monad.Fix(fix)

import Seri.IR

-- elaborate prg
-- Reduce the given expression as much as possible.
elaborate :: Exp -> Exp
elaborate e@(BoolE _) = e
elaborate e@(IntegerE _) = e
elaborate e@(AddE a b) =
    case (elaborate a, elaborate b) of
        (IntegerE av, IntegerE bv) -> IntegerE (av+bv)
        (ea, eb) -> AddE ea eb
elaborate e@(MulE a b) =
    case (elaborate a, elaborate b) of
        (IntegerE av, IntegerE bv) -> IntegerE (av*bv)
        (ea, eb) -> MulE ea eb
elaborate e@(SubE a b) =
    case (elaborate a, elaborate b) of
        (IntegerE av, IntegerE bv) -> IntegerE (av-bv)
        (ea, eb) -> SubE ea eb
elaborate e@(LtE a b) =
    case (elaborate a, elaborate b) of
        (IntegerE av, IntegerE bv) -> BoolE (av < bv)
        (ea, eb) -> LtE ea eb
elaborate (IfE t p a b) =
    case (elaborate p) of
        (BoolE True) -> elaborate a
        (BoolE False) -> elaborate b
        p' -> IfE t p' a b
elaborate (AppE t a b) =
    case (elaborate a) of
        (LamE _ name body) -> elaborate $ reduce name (elaborate b) body
        a' -> AppE t a' (elaborate b)
elaborate e@(FixE (FixE_F t n b)) = reduce n e (elaborate b)
elaborate e@(LamE _ _ _) = e
elaborate e@(VarE _ _) = e

-- reduce n v exp
-- Perform beta reduction in exp, replacing occurances of variable n with v.
reduce :: Name -> Exp -> Exp -> Exp
reduce n v e@(BoolE _) = e
reduce n v e@(IntegerE _) = e
reduce n v (AddE a b) = AddE (reduce n v a) (reduce n v b)
reduce n v (MulE a b) = MulE (reduce n v a) (reduce n v b)
reduce n v (SubE a b) = SubE (reduce n v a) (reduce n v b)
reduce n v (LtE a b) = LtE (reduce n v a) (reduce n v b)
reduce n v (IfE t p a b) = IfE t (reduce n v p) (reduce n v a) (reduce n v b)
reduce n v (AppE t a b) = AppE t (reduce n v a) (reduce n v b)
reduce n v e@(FixE (FixE_F t ln b)) =
    if ln /= n then FixE (FixE_F t ln (reduce n v b)) else e
reduce n v e@(LamE t ln b) =
    if ln /= n then LamE t ln (reduce n v b) else e
reduce n v e@(VarE t vn) =
    if vn == n then v else e

