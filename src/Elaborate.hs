
module Elaborate (
    elaborate
    ) where

import Seri

-- elaborate prg
-- Reduce the given expression as much as possible.
elaborate :: Exp -> Exp
elaborate e@(IntegerE _) = e
elaborate (AddE a b) =
    let ae = elaborate a
        be = elaborate b
    in case (ae, be) of
        (IntegerE av, IntegerE bv) -> IntegerE (av+bv)
        _ -> AddE ae be

elaborate (MulE a b) =
    let ae = elaborate a
        be = elaborate b
    in case (ae, be) of
        (IntegerE av, IntegerE bv) -> IntegerE (av*bv)
        _ -> MulE ae be

elaborate (AppE t f x) =
    let fe = elaborate f
        xe = elaborate x
    in case (fe) of
        (LamE _ _ name body) -> elaborate $ reduce body name xe
        _ -> AppE t fe xe

elaborate e@(LamE _ _ _ _) = e
elaborate e@(VarE _ _) = e


-- reduce exp n v
-- Perform beta reduction in exp, replacing occurances of variable n with v.
reduce :: Exp -> Name -> Exp -> Exp
reduce e@(IntegerE _) _ _ = e
reduce (AddE a b) n v = AddE (reduce a n v) (reduce b n v)
reduce (MulE a b) n v = MulE (reduce a n v) (reduce b n v)
reduce (AppE t a b) n v = AppE t (reduce a n v) (reduce b n v)
reduce (LamE ta tb ln body) n v | ln /= n = LamE ta tb ln (reduce body n v)
reduce e@(LamE _ _ _ _) _ _ = e
reduce (VarE t vn) n v | vn == n = v
reduce e@(VarE t vn) _ _ = e

