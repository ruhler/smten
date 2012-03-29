
module Seri.Elaborate (
    elaborate
    ) where

import Seri.IR

-- elaborate decls prg
-- Reduce the given expression as much as possible.
--  decls - gives the context under which to evaluate the expression.
--  prg - is the expression to evaluate.
elaborate :: [Dec] -> Exp -> Exp
elaborate decls =
    let elab :: Exp -> Exp
        elab e@(IntegerE _) = e
        elab e@(PrimE _ _) = e
        elab (AppE t1 (AppE t2 (PrimE t3 AddP) a) b) =
            case (elab a, elab b) of
                (IntegerE av, IntegerE bv) -> IntegerE (av+bv)
                (ea, eb) -> AppE t1 (AppE t2 (PrimE t3 AddP) ea) eb
        elab (AppE t1 (AppE t2 (PrimE t3 SubP) a) b) =
            case (elab a, elab b) of
                (IntegerE av, IntegerE bv) -> IntegerE (av-bv)
                (ea, eb) -> AppE t1 (AppE t2 (PrimE t3 SubP) ea) eb
        elab (AppE t1 (AppE t2 (PrimE t3 MulP) a) b) =
            case (elab a, elab b) of
                (IntegerE av, IntegerE bv) -> IntegerE (av*bv)
                (ea, eb) -> AppE t1 (AppE t2 (PrimE t3 MulP) ea) eb
        elab (AppE t1 (AppE t2 (PrimE t3 LtP) a) b) =
            case (elab a, elab b) of
                (IntegerE av, IntegerE bv) ->
                    if av < bv
                        then PrimE BoolT TrueP
                        else PrimE BoolT FalseP
                (ea, eb) -> AppE t1 (AppE t2 (PrimE t3 LtP) ea) eb
        elab e@(AppE _ (PrimE _ FixP) (LamE _ n b)) = reduce n e (elab b)
        elab (IfE t p a b) =
            case (elab p) of
                (PrimE _ TrueP) -> elab a
                (PrimE _ FalseP) -> elab b
                p' -> IfE t p' a b
        elab (AppE t a b) =
            case (elab a) of
                (LamE _ name body) -> elab $ reduce name (elab b) body
                a' -> AppE t a' (elab b)
        elab e@(LamE _ _ _) = e
        elab e@(VarE _ nm)
            = case (lookupvar nm decls) of
                 Nothing -> e
                 Just (ValD _ _ ve) -> elab ve
    in elab

-- reduce n v exp
-- Perform beta reduction in exp, replacing occurances of variable n with v.
reduce :: Name -> Exp -> Exp -> Exp
reduce n v e@(IntegerE _) = e
reduce n v e@(PrimE _ _) = e
reduce n v (IfE t p a b) = IfE t (reduce n v p) (reduce n v a) (reduce n v b)
reduce n v (AppE t a b) = AppE t (reduce n v a) (reduce n v b)
reduce n v e@(LamE t ln b) =
    if ln /= n then LamE t ln (reduce n v b) else e
reduce n v e@(VarE t vn) =
    if vn == n then v else e

