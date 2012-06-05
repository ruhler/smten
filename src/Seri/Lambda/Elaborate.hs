
module Seri.Lambda.Elaborate (
    Rule(..), rules, elaborate, coreR,
    ) where

import Data.Generics

import Seri.Lambda.IR
import Seri.Lambda.Env

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
      (CaseE x ms) | (runmeenv e gr x) /= Nothing -> do
         x' <- runmeenv e gr x
         return $ CaseE x' ms
      (CaseE x ((Match p b):ms))
         -> case (match p x) of
                Failed -> Just $ CaseE x ms
                Succeeded vs -> Just $ reduces vs b
                _ -> Nothing
      (AppE (LamE (Sig name _) body) b) -> Just $ reduce name b body
      (AppE a b) | (runmeenv e gr a) /= Nothing -> do
          a' <- runmeenv e gr a
          return $ AppE a' b
      (AppE a b) | (runmeenv e gr b) /= Nothing -> do
          b' <- runmeenv e gr b
          return $ AppE a b'
      (LamE _ _) -> Nothing
      v@(VarE (Sig _ ct) _)
        -> case (lookupvar $ withenv e v) of
               Nothing -> Nothing
               Just (pt, ve) -> Just $ treduces (tmatch pt ct) ve
      _ -> Nothing

data MatchResult = Failed | Succeeded [(Name, Exp)] | Unknown

-- Match an expression against a pattern.
match :: Pat -> Exp -> MatchResult
match (ConP (Sig nm _)) (ConE (Sig n _)) | n == nm = Succeeded []
match (IntegerP i) (IntegerE i') | i == i' = Succeeded []
match (VarP (Sig nm _)) e = Succeeded [(nm, e)]
match (AppP a b) (AppE ae be)
  = case (match a ae, match b be) of
        (Succeeded as, Succeeded bs) -> Succeeded (as ++ bs)
        (Failed, _) -> Failed
        (_, Failed) -> Failed
        _ -> Unknown
match (WildP _) _ = Succeeded []
match _ x | iswhnf x = Failed
match _ _ = Unknown

-- iswhnf exp
--  Return True if the expression is in weak head normal form.
--  TODO: how should we handle primitives?
iswhnf :: Exp -> Bool
iswhnf (IntegerE _) = True
iswhnf (LamE _ _) = True
iswhnf x
 = let iscon :: Exp -> Bool
       iscon (ConE _) = True
       iscon (AppE f _) = iscon f
       iscon _ = False
   in iscon x

-- reduce n v exp
-- Perform beta reduction in exp, replacing occurrences of variable n with v.
reduce :: Name -> Exp -> Exp -> Exp
reduce n v e = reduces [(n, v)] e

-- reduces vs exp
-- Perform multiple simultaneous beta reduction in exp, replacing occurrences
-- of variable n with v if (n, v) is in vs.
reduces :: [(Name, Exp)] -> Exp -> Exp
reduces _ e@(IntegerE _) = e
reduces _ e@(PrimE _) = e
reduces vs (CaseE e ms) =
    let reducematch :: Match -> Match
        reducematch (Match p b) = Match p (reduces vs b)
    in CaseE (reduces vs e) (map reducematch ms)
reduces vs (AppE a b) = AppE (reduces vs a) (reduces vs b)
reduces vs e@(LamE (Sig ln t) b) = LamE (Sig ln t) (reduces (filter (\(n, _) -> n /= ln) vs) b)
reduces _ e@(ConE _) = e
reduces vs e@(VarE (Sig vn _) _) =
    case lookup vn vs of
        (Just v) -> v
        Nothing -> e

-- tmatch poly concrete
-- Given a polymorphic type and a concrete type of the same form, return the
-- mapping from type variable name to concrete type.
tmatch :: Type -> Type -> [(Name, Type)]
tmatch (VarT n) t = [(n, t)]
tmatch (AppT a b) (AppT a' b') = (tmatch a a') ++ (tmatch b b')
tmatch (ForallT _ _ t) t' = tmatch t t'
tmatch _ _ = []

-- treduces vs x
--  Replace each occurence of a variable type according to the given mapping
--  in x.
treduces :: (Data a) => [(Name, Type)] -> a -> a
treduces m =
  let base :: Type -> Type
      base t@(VarT n) = 
        case lookup n m of
            Just t' -> t'
            Nothing -> t
      base t = t
  in everywhere $ mkT base

