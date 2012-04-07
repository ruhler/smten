
module Seri.Elaborate (
    Rule(..), rules, elaborate, coreR,
    ) where

import Seri.IR
import Seri.Typed(seritype)
import Seri.Ppr

-- A reduction rule. Given a set of global declarations, a global reduction
-- rule, and an expression, reduce the expression in some way. Returns Nothing
-- if the rule can't reduce the expression any, otherwise returns a reduced
-- version of the expression.  It need not be fully reduced or anything like
-- that, just reduced in some part.
data Rule = Rule {
    run :: [Dec] -> Rule -> Exp -> Maybe Exp
}

-- Combine a bunch of reduction rules.
-- It tries each rule in turn, applying the first one which succeeds.
rules :: [Rule] -> Rule
rules [] = Rule $ \_ _ _ -> Nothing
rules (r:rs) = Rule $ \decls gr e ->
  case run r decls gr e of
      Just e' -> Just e'
      Nothing -> run (rules rs) decls gr e

-- elaborate decls prg
-- Reduce the given expression as much as possible.
--  rule - the reduction rule to use
--  decls - gives the context under which to evaluate the expression.
--  prg - is the expression to evaluate.
elaborate :: Rule -> [Dec] -> Exp -> Exp
elaborate r decls prg =
    case run r decls r prg of
        Just e -> elaborate r decls e
        Nothing -> prg

-- coreR - The core reduction rules.
coreR :: Rule
coreR = Rule $ \decls gr e ->
   case e of
      (AppE _ (PrimE _ "fix") (LamE _ n b))
        -> Just $ reduce n e b
      (IfE _ p a b) | p == trueE -> Just a
      (IfE _ p a b) | p == falseE -> Just b
      (IfE t p a b) -> do
         p' <-  run gr decls gr p
         return $ IfE t p' a b
      (CaseE t x ((Match p b):ms))
         -> case (match p x) of
                Failed -> Just $ CaseE t x ms
                Succeeded vs -> Just $ reduces vs b
                _ -> Nothing
      (AppE _ (LamE _ name body) b) -> Just $ reduce name b body
      (AppE t a b) | (run gr decls gr a) /= Nothing -> do
          a' <- run gr decls gr a
          return $ AppE t a' b
      (AppE t a b) | (run gr decls gr b) /= Nothing -> do
          b' <- run gr decls gr b
          return $ AppE t a b'
      (LamE _ _ _) -> Nothing
      (VarE _ nm)
        -> case (lookupvar nm decls) of
               Nothing -> Nothing
               Just (ValD _ _ ve) -> Just ve
      _ -> Nothing

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
