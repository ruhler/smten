
module Seri.Elaborate (
    elaborate
    ) where

import Seri.IR
import Seri.Typed(seritype)
import Seri.Ppr

-- elaborate decls prg
-- Reduce the given expression as much as possible.
--  decls - gives the context under which to evaluate the expression.
--  prg - is the expression to evaluate.
elaborate :: [Dec] -> Exp -> Exp
elaborate decls =
    -- elab returns (True, e) if some reduction happened, (False, e) else.
    -- elab only makes partial progress. It doesn't ensure the result is fully
    -- elaborated.
    let elab :: Exp -> (Bool, Exp)
        elab (AppE _ (AppE _ (PrimE _ AddP) (IntegerE a)) (IntegerE b))
            = (True, IntegerE (a+b))
        elab (AppE _ (AppE _ (PrimE _ SubP) (IntegerE a)) (IntegerE b))
            = (True, IntegerE (a-b))
        elab (AppE _ (AppE _ (PrimE _ MulP) (IntegerE a)) (IntegerE b))
            = (True, IntegerE (a*b))
        elab (AppE _ (AppE _ (PrimE _ LtP) (IntegerE a)) (IntegerE b))
            = let ne = if a < b
                         then PrimE (seritype True) TrueP
                         else PrimE (seritype False) FalseP
              in (True, ne)
        elab e@(AppE _ (PrimE _ FixP) (LamE _ n b))
            = (True, reduce n e b)
        elab (IfE _ (PrimE _ TrueP) a b) = (True, a)
        elab (IfE _ (PrimE _ FalseP) a b) = (True, b)
        elab (IfE t p a b) | fst (elab p) = (True, IfE t (snd (elab p)) a b)
        elab (AppE _ (LamE _ name body) b) = (True, reduce name b body)
        elab (AppE t a b) | fst (elab a) = (True, AppE t (snd (elab a)) b)
        elab (AppE t a b) | fst (elab b) = (True, AppE t a (snd (elab b)))
        elab e@(LamE _ _ _) = (False, e)
        elab e@(VarE _ nm)
            = case (lookupvar nm decls) of
                 Nothing -> (False, e)
                 Just (ValD _ _ ve) -> (True, ve)
        elab e = (False, e)

        -- perform full elaboration of an expression.
        elabfull :: Exp -> Exp
        elabfull e | fst $ elab e = elabfull (snd $ elab e)
        elabfull e = e
    in elabfull

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

