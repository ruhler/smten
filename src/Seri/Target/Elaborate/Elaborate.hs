
module Seri.Target.Elaborate.Elaborate (
    Rule(..), rules, elaborate, coreR,
    ) where

import Data.Generics

import Seri.Failable
import Seri.Lambda

-- A reduction rule. Given a set of global declarations, a global reduction
-- rule, and an expression, reduce the expression in some way. Returns Nothing
-- if the rule can't reduce the expression any, otherwise returns a reduced
-- version of the expression.  It need not be fully reduced or anything like
-- that, just reduced in some part.
data Rule m = Rule {
    run :: RuleBody m
}

type RuleBody m = (Rule m -> Env Exp -> m (Maybe Exp))

runme :: Rule m -> Env Exp -> m (Maybe Exp)
runme r = run r r

runmeenv :: Env x -> Rule m -> Exp -> m (Maybe Exp)
runmeenv e r x = runme r (withenv e x)

-- Combine a bunch of reduction rules.
-- It tries each rule in turn, applying the first one which succeeds.
rules :: (Monad m) => [Rule m] -> Rule m
rules [] = Rule $ \_ _ -> return Nothing
rules (r:rs) = Rule $ \gr e -> do
  x <- run r gr e
  case x of
      Just e' -> return $ Just e'
      Nothing -> run (rules rs) gr e

-- elaborate decls prg
-- Reduce the given expression as much as possible.
--  rule - the reduction rule to use
--  decls - gives the context under which to evaluate the expression.
--  prg - is the expression to evaluate.
elaborate :: (Monad m) => Rule m -> Env Exp -> m Exp
elaborate r prg = do
    x <- runme r prg
    case x of
        Just e -> elaborate r (withenv prg e)
        Nothing -> return $ val prg

-- coreR - The core reduction rules.
coreR :: (Monad m) => Rule m
coreR = rules [casesubR, caseredR, appredR, applsubR, apprsubR, varredR]

casesubR :: (Monad m) => Rule m
casesubR = Rule $ \gr e ->
   case val e of
      (CaseE x ms) -> do
         x' <- runmeenv e gr x
         return $ do
            vx' <- x'
            return $ CaseE vx' ms
      _ -> return Nothing

caseredR :: (Monad m) => Rule m
caseredR = Rule $ \gr e ->
   case val e of
      (CaseE x ((Match p b):ms))
         -> case (match p x) of
                Failed -> return . Just $ CaseE x ms
                Succeeded vs -> return . Just $ reduces vs b
                _ -> return Nothing
      _ -> return Nothing

appredR :: (Monad m) => Rule m
appredR = Rule $ \gr e ->
   case val e of
      (AppE (LamE (Sig name _) body) b)
         -> return . Just $ reduce name b body
      _ -> return Nothing

applsubR :: (Monad m) => Rule m
applsubR = Rule $ \gr e ->
   case val e of
      (AppE a b) -> do
          a' <- runmeenv e gr a
          return $ do
              va' <- a'
              return $ AppE va' b
      _ -> return Nothing

apprsubR :: (Monad m) => Rule m
apprsubR = Rule $ \gr e ->
   case val e of
      (AppE a b) -> do
          b' <- runmeenv e gr b
          return $ do
              vb' <- b'
              return $ AppE a vb'
      _ -> return Nothing

varredR :: (Monad m) => Rule m
varredR = Rule $ \gr e ->
   case val e of
      (VarE s@(Sig _ ct))
        -> case (attemptM $ lookupVar (withenv e s)) of
               Nothing -> return Nothing
               Just (pt, ve) -> return . Just $ assign (assignments pt ct) ve
      _ -> return Nothing
        
data MatchResult = Failed | Succeeded [(Name, Exp)] | Unknown

-- Match an expression against a pattern.
match :: Pat -> Exp -> MatchResult
match (ConP _ nm []) (ConE (Sig n _)) | n == nm = Succeeded []
match (ConP t n ps) (AppE ae be) | not (null ps)
  = case (match (ConP t n (init ps)) ae, match (last ps) be) of
        (Succeeded as, Succeeded bs) -> Succeeded (as ++ bs)
        (Failed, _) -> Failed
        (Succeeded _, Failed) -> Failed
        _ -> Unknown
match (IntegerP i) (IntegerE i') | i == i' = Succeeded []
match (VarP (Sig nm _)) e = Succeeded [(nm, e)]
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
reduces vs e@(VarE (Sig vn _)) =
    case lookup vn vs of
        (Just v) -> v
        Nothing -> e

