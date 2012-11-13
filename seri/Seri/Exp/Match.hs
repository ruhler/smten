
-- | Syntactic sugar involving pattern matching.
module Seri.Exp.Match (
    Pat(..), SMatch(..), MMatch(..),
    tupleP, listP,
    caseE, clauseE, mlamE, mletE, mletsE,
    ) where

import Data.Maybe(fromMaybe)

import Seri.Lit
import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.Fresh
import Seri.Exp.Exp
import Seri.Exp.Sugar

data Pat = ConP Name [Pat]
         | VarP Name
         | LitP Lit
         | WildP
    deriving (Eq, Show)

listP :: [Pat] -> Pat
listP [] = ConP (name "[]") []
listP (x:xs) = ConP (name ":") [x, listP xs]

tupleP :: [Pat] -> Pat
tupleP ps = ConP (tupleN (length ps)) ps

-- | Single argument match
data SMatch = SMatch Pat Exp

-- | Multi-argument match
data MMatch = MMatch [Pat] Exp

-- | Desugar a match expression. That is, a case statement with a single
-- match and a default clause.
-- 
-- case x of
--     p -> yv
--     _ -> n
matchE :: (Fresh f) => Exp -> SMatch -> Exp -> f Exp
matchE _ (SMatch WildP yv) _ = return yv
matchE x (SMatch (VarP n) yv) _ = return $ appE (lamE (Sig n UnknownT) yv) x
matchE x (SMatch (LitP l) yv) n =
  let p = appsE (varE (Sig (name "==") UnknownT)) [litE l, x]
  in return $ ifE p yv n
matchE x (SMatch (ConP nm ps) yv) n =
  let -- case x of
      --    K pa pb pc -> yv
      --    _ -> n
      -- 
      -- Translates to:
      -- case x of
      --    K -> \a b c -> 
      --            case a of
      --               pa -> case b of
      --                       pb -> case c of 
      --                               pc -> yv
      --                               _ -> n
      --                       _ -> n
      --               _ -> n
      --    _ -> n
      mkcases :: (Fresh m) => [(Pat, Exp)] -> Exp -> Exp -> m Exp
      mkcases [] y n = return y
      mkcases ((p, x):ps) y n = do
        body <- mkcases ps y n
        matchE x (SMatch p body) n
  in do
      vars <- mapM fresh [Sig (name $ "_p" ++ show i) UnknownT | i <- [1..(length ps)]]
      body <- mkcases [(p, varE v) | (p, v) <- zip ps vars] yv n
      return $ CaseE x (Sig nm UnknownT) (lamsE vars body) n

-- | Desugar multiple matches. Or, in other words, a case statement with an
-- explicit default clause.
-- case x of
--   p1 -> b1
--   p2 -> b2
--   ...
--   _ -> n
matchesE :: (Fresh f) => Exp -> [SMatch] -> Exp -> f Exp
matchesE e [m] n = matchE e m n
matchesE e (m:ms) n = do
    n' <- matchesE e ms n
    matchE e m n'

-- | Desugar a case expression.
caseE :: Exp -> [SMatch] -> Exp
caseE x ms = runFreshPretty $ matchesE x ms (errorE "case no match")

-- | Single argument clause expression
sclauseE :: [SMatch] -> Exp
sclauseE ms =
  let x = Sig (name "_x") UnknownT
  in lamE x $ caseE (varE x) ms

-- | Multi-argument clause expression
-- This converts multi-arg clause expressions to single-arg clause
-- expressions, then calls sclauseE.
--
--  case of
--    p1a, p1b, p1c -> m1
--    p2a, p2b, p2c -> m2
--
-- Is converted into:
--   (curry (curry (
--      case of
--         ((p1a, p1b), p1c) -> m1
--         ((p2a, p2b), p2c) -> m1
clauseE :: [MMatch] -> Exp
clauseE (MMatch [] b : _) = b
clauseE ms@(MMatch ps _ : _) = 
  let tupp :: Pat -> Pat -> Pat
      tupp a b = tupleP [a, b]

      repat :: [Pat] -> Pat
      repat = foldl1 tupp

      -- Apply curry to the given expression n times.
      curryn :: Int -> Exp -> Exp
      curryn n e | n < 0 = error $ "curryn with " ++ show n
      curryn 0 e = e
      curryn n e = curryn (n-1) (curryE e)

      -- Apply curry to the given expression.
      curryE :: Exp -> Exp
      curryE e = appE (varE (Sig (name "curry") UnknownT)) e

      sms = [SMatch (repat ps) b | MMatch ps b <- ms]
  in curryn (length ps - 1) (sclauseE sms)


-- | Lambda with pattern matching.
mlamE :: MMatch -> Exp
mlamE m = clauseE [m]

-- | Let with pattern matching
mletE :: Pat -> Exp -> Exp -> Exp
mletE  p v e = caseE v [SMatch p e]

-- | Sequential let with pattern matching
mletsE :: [(Pat, Exp)] -> Exp -> Exp
mletsE [] x = x
mletsE ((p, v):ps) x = mletE p v (mletsE ps x)

