
module Seri.Elaborate (
    Rule(..), rules, elaborate, coreR,
    ) where

import Seri.IR
import Seri.Env
import Seri.Typed(seritype)
import Seri.Lib.Bool

-- A reduction rule. Given a set of global declarations, a global reduction
-- rule, and an expression, reduce the expression in some way. Returns Nothing
-- if the rule can't reduce the expression any, otherwise returns a reduced
-- version of the expression.  It need not be fully reduced or anything like
-- that, just reduced in some part.
data Rule = Rule {
    run :: Rule -> Env Exp -> Maybe Exp
}

runme :: Rule -> Env Exp -> Maybe Exp
runme r = run r r

runmeenv :: Env x -> Rule -> Exp -> Maybe Exp
runmeenv e r x = runme r (withenv e x)

-- Combine a bunch of reduction rules.
-- It tries each rule in turn, applying the first one which succeeds.
rules :: [Rule] -> Rule
rules [] = Rule $ \_ _ -> Nothing
rules (r:rs) = Rule $ \gr e ->
  case run r gr e of
      Just e' -> Just e'
      Nothing -> run (rules rs) gr e

-- elaborate decls prg
-- Reduce the given expression as much as possible.
--  rule - the reduction rule to use
--  decls - gives the context under which to evaluate the expression.
--  prg - is the expression to evaluate.
elaborate :: Rule -> Env Exp -> Exp
elaborate r prg =
    case runme r prg of
        Just e -> elaborate r (withenv prg e)
        Nothing -> val prg

-- coreR - The core reduction rules.
coreR :: Rule
coreR = Rule $ \gr e ->
   case val e of
      (IfE _ p a b) | p == trueE -> Just a
      (IfE _ p a b) | p == falseE -> Just b
      (IfE t p a b) -> do
         p' <-  runmeenv e gr p
         return $ IfE t p' a b
      (CaseE t x ms) | (runmeenv e gr x) /= Nothing -> do
         x' <- runmeenv e gr x
         return $ CaseE t x' ms
      (CaseE t x ((Match p b):ms))
         -> case (match p x) of
                Failed -> Just $ CaseE t x ms
                Succeeded vs -> Just $ reduces vs b
                _ -> Nothing
      (AppE _ (LamE _ name body) b) -> Just $ reduce name b body
      (AppE t a b) | (runmeenv e gr a) /= Nothing -> do
          a' <- runmeenv e gr a
          return $ AppE t a' b
      (AppE t a b) | (runmeenv e gr b) /= Nothing -> do
          b' <- runmeenv e gr b
          return $ AppE t a b'
      (LamE _ _ _) -> Nothing
      (VarE _ nm)
        -> case (lookupvar nm e) of
               Nothing -> Nothing
               Just (ValD _ _ ve) -> Just ve
      _ -> Nothing

data MatchResult = Failed | Succeeded [(Name, Exp)] | Unknown

-- Match an expression against a pattern.
match :: Pat -> Exp -> MatchResult
match (ConP nm) (ConE _ n) | n == nm = Succeeded []
match (IntegerP i) (IntegerE i') | i == i' = Succeeded []
match (VarP nm) e = Succeeded [(nm, e)]
match (AppP a b) (AppE _ ae be)
  = case (match a ae, match b be) of
        (Succeeded as, Succeeded bs) -> Succeeded (as ++ bs)
        (Failed, _) -> Failed
        (_, Failed) -> Failed
        _ -> Unknown
match WildP _ = Succeeded []
match _ x | iswhnf x = Failed
match _ _ = Unknown

-- iswhnf exp
--  Return True if the expression is in weak head normal form.
--  TODO: how should we handle primitives?
iswhnf :: Exp -> Bool
iswhnf (IntegerE _) = True
iswhnf (LamE _ _ _) = True
iswhnf x
 = let iscon :: Exp -> Bool
       iscon (ConE _ _) = True
       iscon (AppE _ f _) = iscon f
       iscon _ = False
   in iscon x

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
