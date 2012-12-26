
-- | Syntactic sugar involving pattern matching.
module Seri.Exp.Match (
    Pat(..), SMatch(..), MMatch(..),
    tupleP, listP, charP, stringP,
    mcaseE, clauseE, mlamE, mletE, mletsE,
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
         | AsP Name Pat
         | LitP Lit
         | WildP
    deriving (Eq, Show)

listP :: [Pat] -> Pat
listP [] = ConP (name "[]") []
listP (x:xs) = ConP (name ":") [x, listP xs]

charP :: Char -> Pat
charP = LitP . charL

stringP :: String -> Pat
stringP = listP . map charP

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
matchE x (SMatch (AsP nm p) yv) n = do
    rest <- matchE x (SMatch p yv) n
    return $ letE (Sig nm (typeof x)) x rest
matchE x (SMatch (LitP l) yv) n =
  let p = appsE (varE (Sig (name "==") UnknownT)) [litE l, x]
  in return $ ifE p yv n
matchE x (SMatch (ConP nm ps) yv) n | isSimple n = do
      y <- clauseE' [MMatch ps yv] n
      return $ CaseE x (Sig nm UnknownT) y n
matchE x m n = do
  nv <- fresh (Sig (name "_n") UnknownT)
  body <- matchE x m (varE nv)
  return $ letE nv n body

-- | Desugar multiple matches. Or, in other words, a case statement with an
-- explicit default clause.
-- case x of
--   p1 -> b1
--   p2 -> b2
--   ...
--   _ -> n
matchesE :: (Fresh f) => Exp -> [SMatch] -> Exp -> f Exp
matchesE e [m] n = matchE e m n
matchesE e (m:ms) n | isSimple e = do
    n' <- matchesE e ms n
    matchE e m n'
matchesE e ms n = do
    ev <- fresh (Sig (name "_e") UnknownT)
    body <- matchesE (varE ev) ms n
    return $ letE ev e body

-- | Desugar a case expression.
mcaseE :: Exp -> [SMatch] -> Exp
mcaseE x ms = runFreshPretty $ matchesE x ms (errorE "case no match")

clauseE :: [MMatch] -> Exp
clauseE ms = runFreshPretty $ clauseE' ms (errorE "case no match")

clauseE' :: (Fresh f) => [MMatch] -> Exp -> f Exp
clauseE' [MMatch ps e] n = do
  -- If we are only making one match, we pick the variables for the lambda
  -- more wisely to avoid silly things like:
  --    \_p1 -> let a = _p1
  --            in foo a
  -- This doesn't work if there are multiple matches because there are scoping
  -- issues then.
  let mkvar :: (Fresh f) => Pat -> f (Sig, Pat)
      mkvar (VarP n) = return (Sig n UnknownT, WildP)
      mkvar (AsP n p) = return (Sig n UnknownT, p)
      mkvar p = do  
        s <- fresh $ Sig (name "_p") UnknownT
        return (s, p)
  pvs <- mapM mkvar ps
  let (vars, ps') = unzip pvs
  b <- mmatchE vars (MMatch ps' e) n
  return $ lamsE vars b

clauseE' ms@(MMatch ps _ : _) n = do
    vars <- mapM fresh [Sig (name $ "_p" ++ show i) UnknownT | i <- [1..(length ps)]]
    b <- mmatchesE vars ms n
    return $ lamsE vars b

mmatchesE :: (Fresh f) => [Sig] -> [MMatch] -> Exp -> f Exp
mmatchesE args [m] n = mmatchE args m n
mmatchesE args (m:ms) n = do
    n' <- mmatchesE args ms n
    mmatchE args m n'

mmatchE :: (Fresh f) => [Sig] -> MMatch -> Exp -> f Exp
mmatchE args (MMatch ps yv) n =
  let -- case a b c of
      --    pa pb pc -> yv
      --    _ -> n
      --
      -- Translates to:
      --  \a b c -> 
      --     case a of
      --        pa -> case b of
      --                pb -> case c of 
      --                        pc -> yv
      --                        _ -> n
      --                _ -> n
      --        _ -> n
      mkcases :: (Fresh m) => [(Pat, Exp)] -> Exp -> Exp -> m Exp
      mkcases [] y n = return y
      mkcases ((p, x):ps) y n = do
        body <- mkcases ps y n
        matchE x (SMatch p body) n
  in mkcases (zip ps (map varE args)) yv n

-- | Lambda with pattern matching.
mlamE :: MMatch -> Exp
mlamE m = clauseE [m]

-- | Let with pattern matching
mletE :: Pat -> Exp -> Exp -> Exp
mletE  p v e = mcaseE v [SMatch p e]

-- | Sequential let with pattern matching
mletsE :: [(Pat, Exp)] -> Exp -> Exp
mletsE [] x = x
mletsE ((p, v):ps) x = mletE p v (mletsE ps x)

-- Return true if the expression is simple.
-- If an expression is simple, there's no cost to duplicating it.
isSimple :: Exp -> Bool
isSimple (AppE {}) = False
isSimple (LamE {}) = False
isSimple (CaseE {}) = False
isSimple _ = True

