
-- | Syntactic sugar involving pattern matching.
module Seri.Exp.Match (
    Pat(..), SMatch(..), MMatch(..),
    tupleP, listP,
    caseE, clauseE, mlamE, mletE, mletsE,
    ) where

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
  in error $ "TODO: matchE ConP"

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
caseE x ms = runFreshPretty $ matchesE x ms (errorE UnknownT "case no match")

-- | Single argument clause expression
sclauseE :: (Fresh m) => [SMatch] -> m Exp
sclauseE = error $ "TODO: sclauseE"

-- | Multi-argument clause expression
clauseE :: [MMatch] -> Exp
clauseE = error $ "TODO: clauseE"

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

