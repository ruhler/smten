
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
        elab (AppE _ (AppE _ (PrimE _ "+") (IntegerE a)) (IntegerE b))
            = (True, IntegerE (a+b))
        elab (AppE _ (AppE _ (PrimE _ "-") (IntegerE a)) (IntegerE b))
            = (True, IntegerE (a-b))
        elab (AppE _ (AppE _ (PrimE _ "*") (IntegerE a)) (IntegerE b))
            = (True, IntegerE (a*b))
        elab (AppE _ (AppE _ (PrimE _ "<") (IntegerE a)) (IntegerE b))
            = let ne = if a < b
                         then trueE
                         else falseE
              in (True, ne)
        elab e@(AppE _ (PrimE _ "fix") (LamE _ n b))
            = (True, reduce n e b)
        elab (IfE _ p a b) | p == trueE = (True, a)
        elab (IfE _ p a b) | p == falseE = (True, b)
        elab (IfE t p a b) | fst (elab p) = (True, IfE t (snd (elab p)) a b)
        elab c@(CaseE t e ((Match p b):ms)) = 
            case (match p e) of
                Failed -> (True, CaseE t e ms)
                Succeeded vs -> (True, reduces vs b)
                _ -> (False, c)
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

data MatchResult = Failed | Succeeded [(Name, Exp)] | Unknown

-- Match an expression against a pattern.
match :: Pat -> Exp -> MatchResult
match (ConP nm) (IntegerE _) = Failed
match (ConP nm) (LamE _ _ _) = Failed
match (ConP nm) (ConE _ n) | n == nm = Succeeded []
match (ConP nm) (ConE _ n) = Failed
match (ConP nm) _ = Unknown
match (VarP nm) e = Succeeded [(nm, e)]
match (AppP a b) (IntegerE _) = Failed
match (AppP a b) (LamE _ _ _) = Failed
match (AppP a b) (ConE _ _) = Failed
match (AppP a b) (AppE _ ae be)
  = case (match a ae, match b be) of
        (Succeeded as, Succeeded bs) -> Succeeded (as ++ bs)
        (Failed, _) -> Failed
        (_, Failed) -> Failed
        _ -> Unknown
match (AppP a b) _ = Unknown
match WildP _ = Succeeded []


-- reduce n v exp
-- Perform beta reduction in exp, replacing occurances of variable n with v.
reduce :: Name -> Exp -> Exp -> Exp
reduce n v e = reduces [(n, v)] e

-- reduces vs exp
-- Perform multiple simultaneous beta reduction in exp, replacing occurances
-- of variable n with v if (n, v) is in vs.
reduces :: [(Name, Exp)] -> Exp -> Exp
reduces _ e@(IntegerE _) = e
reduces _ e@(PrimE _ _) = e
reduces vs (IfE t p a b) = IfE t (reduces vs p) (reduces vs a) (reduces vs b)
reduces vs (CaseE t e ms) =
    let reducematch :: Match -> Match
        reducematch (Match p b) = Match p (reduces vs b)
    in CaseE t (reduces vs e) (map reducematch ms)
reduces vs (AppE t a b) = AppE t (reduces vs a) (reduces vs b)
reduces vs e@(LamE t ln b) = LamE t ln (reduces (filter (\(n, _) -> n /= ln) vs) b)
reduces _ e@(ConE _ _) = e
reduces vs e@(VarE t vn) =
    case lookup vn vs of
        (Just v) -> v
        Nothing -> e
