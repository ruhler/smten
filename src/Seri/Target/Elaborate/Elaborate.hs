
-- | Target for reducing a seri expression to a normal form.
module Seri.Target.Elaborate.Elaborate (
    Rule(..), rules, elaborate, coreR, simplifyR,
    ) where

import Data.Generics
import Data.Maybe(catMaybes, fromMaybe)
import Data.List(nub, (\\))

import Seri.Failable
import Seri.Lambda

-- | A reduction rule. Given a set of global declarations, a global reduction
-- rule, and an expression, reduce the expression in some way. Returns Nothing
-- if the rule can't reduce the expression any, otherwise returns a reduced
-- version of the expression.  It need not be fully reduced or anything like
-- that, just reduced in some part.
data Rule m = Rule {
    run :: RuleBody m
}

type RuleBody m = (Rule m -> Env -> Exp -> m (Maybe Exp))

runme :: Rule m -> Env -> Exp -> m (Maybe Exp)
runme r = run r r

-- | Combine a bunch of reduction rules.
-- It tries each rule in turn, applying the first one which succeeds.
rules :: (Monad m) => [Rule m] -> Rule m
rules [] = Rule $ \_ _ _ -> return Nothing
rules (r:rs) = Rule $ \gr env e -> do
  x <- run r gr env e
  case x of
      Just e' -> return $ Just e'
      Nothing -> run (rules rs) gr env e

-- | Reduce the given expression as much as possible.
elaborate :: (Monad m)
          => Rule m   -- ^ reduction rule to use
          -> Env      -- ^ context under which to evaluate the expression
          -> Exp      -- ^ expression to evaluate
          -> m Exp
elaborate r env prg = do
    x <- runme r env prg
    case x of
        Just e -> elaborate r env e
        Nothing -> return prg

-- | The core reduction rule.
coreR :: (Monad m) => Rule m
coreR = rules [casesubR, caseredR, appredR, applsubR, apprsubR, varredR]

-- | The core simplification rule.
-- Simplification simplifies expressions inside lambdas and case statements,
-- but does not perform variable substitution from the environment.
simplifyR :: (Monad m) => Rule m
simplifyR = rules [casesubR, caseredR, casesimpR, appredR, applsubR, apprsubR, lamsimpR]
        

casesubR :: (Monad m) => Rule m
casesubR = Rule $ \gr env e ->
   case e of
      (CaseE x ms) -> do
         x' <- runme gr env x
         return $ do
            vx' <- x'
            return $ CaseE vx' ms
      _ -> return Nothing

casesimpR :: (Monad m) => Rule m
casesimpR = Rule $ \gr env e ->
    case e of
      (CaseE x ms) -> do
           ms' <- sequence [runme gr env b | Match _ b <- ms]
           if null (catMaybes ms')
              then return Nothing
              else return (Just $ CaseE x [Match p (fromMaybe b b') | (Match p b, b') <- zip ms ms'])
      _ -> return Nothing

caseredR :: (Monad m) => Rule m
caseredR = Rule $ \gr env e ->
   case e of
      (CaseE x ((Match p b):ms))
         -> case (match p x) of
                Failed ->
                    -- Don't make a case statement empty, because we want to 
                    -- keep enough information to determine the type of the
                    -- case statement.
                    -- TODO: maybe there's something better we could do?
                    -- return a call to some error primitive or something?
                    if null ms 
                        then return Nothing
                        else return . Just $ CaseE x ms
                Succeeded vs -> return . Just $ reduces vs b
                _ -> return Nothing
      _ -> return Nothing

appredR :: (Monad m) => Rule m
appredR = Rule $ \gr env e ->
   case e of
      (AppE (LamE (Sig name _) body) b)
         -> let body' = alpharename (free b \\ [name]) body
                result = reduce name b body'
            in return $ Just result
      _ -> return Nothing

applsubR :: (Monad m) => Rule m
applsubR = Rule $ \gr env e ->
   case e of
      (AppE a b) -> do
          a' <- runme gr env a
          return $ do
              va' <- a'
              return $ AppE va' b
      _ -> return Nothing

apprsubR :: (Monad m) => Rule m
apprsubR = Rule $ \gr env e ->
   case e of
      (AppE a b) -> do
          b' <- runme gr env b
          return $ do
              vb' <- b'
              return $ AppE a vb'
      _ -> return Nothing

lamsimpR :: (Monad m) => Rule m
lamsimpR = Rule $ \gr env e ->
    case e of
       (LamE s b) -> do
            b' <- runme gr env b
            return $ do
                vb' <- b'
                return $ LamE s vb'
       _ -> return Nothing

varredR :: (Monad m) => Rule m
varredR = Rule $ \gr env e ->
   case e of
      (VarE s@(Sig _ ct))
        -> case (attemptM $ lookupVar env s) of
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
reduces vs (CaseE e ms) =
    let reducematch :: Match -> Match
        reducematch (Match p b) =
         let bound = map fst (bindingsP p)
             vs' = filter (\(n, _) -> not (n `elem` bound)) vs
         in Match p (reduces vs' b)
    in CaseE (reduces vs e) (map reducematch ms)
reduces vs (AppE a b) = AppE (reduces vs a) (reduces vs b)
reduces vs e@(LamE (Sig ln t) b)
  = LamE (Sig ln t) (reduces (filter (\(n, _) -> n /= ln) vs) b)
reduces _ e@(ConE _) = e
reduces vs e@(VarE (Sig vn _)) =
    case lookup vn vs of
        (Just v) -> v
        Nothing -> e

-- | Return a list of all variables in the given expression.
names :: Exp -> [Name]
names (IntegerE {}) = []
names (CaseE e ms) = 
  let namesm :: Match -> [Name]
      namesm (Match p b) = map fst (bindingsP p) ++ names b
  in nub $ concat (names e : map namesm ms)
names (AppE a b) = names a ++ names b
names (LamE (Sig n _) b) = nub $ n : names b
names (ConE {}) = []
names (VarE (Sig n _)) = [n]

-- | Rename any variable bindings in the given expression to names which do
-- not belong to the given list.
alpharename :: [Name] -> Exp -> Exp
alpharename bad e =
  let isgood :: String -> Bool
      isgood s = not (s `elem` bad)

      isgoodnew :: String -> Bool
      isgoodnew s = isgood s && not (s `elem` names e)

      -- get the new name for the given name.
      newname :: String -> String
      newname n | isgood n = n
      newname n = head (filter isgoodnew [n ++ show i | i <- [0..]])
    
      repat :: Pat -> Pat
      repat (ConP t n ps) = ConP t n (map repat ps)
      repat (VarP (Sig n t)) = VarP (Sig (newname n) t)
      repat p@(IntegerP {}) = p
      repat p@(WildP {}) = p

      rematch :: [Name] -> Match -> Match
      rematch bound (Match p b) = 
        let p' = repat p
            b' = rename (map fst (bindingsP p) ++ bound) b
        in Match p' b'

      -- Do alpha renaming in an expression given the list of bound variable
      -- names before renaming.
      rename :: [Name] -> Exp -> Exp
      rename _ e@(IntegerE {}) = e
      rename bound (CaseE e ms)
        = CaseE (rename bound e) (map (rematch bound) ms)
      rename bound (AppE a b) = AppE (rename bound a) (rename bound b)
      rename bound (LamE (Sig n t) b)
        = LamE (Sig (newname n) t) (rename (n : bound) b)
      rename _ e@(ConE {}) = e
      rename bound (VarE (Sig n t)) | n `elem` bound = VarE (Sig (newname n) t) 
      rename _ e@(VarE {}) = e
  in rename [] e

