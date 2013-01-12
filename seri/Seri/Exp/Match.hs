
-- | Syntactic sugar involving pattern matching.
module Seri.Exp.Match (
    Pat(..), Guard(..), Body(..), Alt(..), MAlt(..),
    tupleP, listP, charP, stringP, numberP,
    mcaseE, clauseE, mlamE, mletE, mletsE,
    lcompE, normalB,
    simpleA, simpleMA,
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
         | LitP Exp
         | WildP
    deriving (Eq, Show)

listP :: [Pat] -> Pat
listP [] = ConP (name "[]") []
listP (x:xs) = ConP (name ":") [x, listP xs]

charP :: Char -> Pat
charP = LitP . litE . charL

stringP :: String -> Pat
stringP = listP . map charP

numberP :: Integer -> Pat
numberP = LitP . numberE

tupleP :: [Pat] -> Pat
tupleP ps = ConP (tupleN (length ps)) ps

-- | Perform a pattern match.
-- case x of
--     p -> yv
--     _ -> n
matchpatE :: (Fresh f) => Exp -> Pat -> Exp -> Exp -> f Exp
matchpatE _ WildP yv _ = return yv
matchpatE x (VarP n) yv _ = return $ appE (lamE (Sig n UnknownT) yv) x
matchpatE x (AsP nm p) yv n = do
    rest <- matchpatE x p yv n
    return $ letE (Sig nm (typeof x)) x rest
matchpatE x (LitP e) yv n =
  let p = appsE (varE (Sig (name "==") UnknownT)) [e, x]
  in return $ ifE p yv n
matchpatE x (ConP nm ps) yv n | isSimple n = do
      y <- clauseE' [simpleMA ps yv] n
      return $ CaseE x (Sig nm UnknownT) y n
matchpatE x p y n = do
  nv <- fresh (Sig (name "_n") UnknownT)
  body <- matchpatE x p y (varE nv)
  return $ letE nv n body

data Guard = PatG Pat Exp
           | LetG [(Pat, Exp)]
           | BoolG Exp
    deriving (Eq, Show)

-- | Perform a guard match
--   | g = y
--   | otherwise = n
matchguardE :: (Fresh f) => Guard -> Exp -> Exp -> f Exp
matchguardE (PatG p x) y n = matchpatE x p y n
matchguardE (LetG decls) y _ = return (mletsE decls y)
matchguardE (BoolG x) y n = return (ifE x y n)

-- | Perform multiple guard matches
--   | g1, g2, ... = y
--   | otherwise = n
matchguardsE :: (Fresh f) => [Guard] -> Exp -> Exp -> f Exp
matchguardsE [] y _ = return y 
matchguardsE (g:gs) y n = do
    y' <- matchguardsE gs y n
    matchguardE g y' n

data Body = Body [Guard] Exp
    deriving (Eq, Show)

matchbodyE :: (Fresh f) => Body -> Exp -> f Exp
matchbodyE (Body gs y) n = matchguardsE gs y n

matchbodiesE :: (Fresh f) => [Body] -> Exp -> f Exp
matchbodiesE [] n = return n
matchbodiesE (b:bs) n = do
    n' <- matchbodiesE bs n
    matchbodyE b n'

data Alt = Alt Pat [Body]
    deriving (Eq, Show)

simpleA :: Pat -> Exp -> Alt
simpleA p e = Alt p [Body [] e]

-- Match a single alternative:
--  case x of
--    alt 
--    _ -> n
matchaltE :: (Fresh f) => Exp -> Alt -> Exp -> f Exp
matchaltE x (Alt p bs) n = do
    body <- matchbodiesE bs n
    matchpatE x p body n

-- Match multiple alternatives:
--   case x of
--     alt1 
--     alt2
--     ...
--     _ -> n
matchaltsE :: (Fresh f) => Exp -> [Alt] -> Exp -> f Exp
matchaltsE _ [] n = return n
matchaltsE x (a:as) n | isSimple x = do
    n' <- matchaltsE x as n
    matchaltE x a n'
matchaltsE x as n = do
    xv <- fresh (Sig (name "_x") UnknownT)
    body <- matchaltsE (varE xv) as n
    return $ letE xv x body

data MAlt = MAlt [Pat] [Body]
    deriving (Eq, Show)

simpleMA :: [Pat] -> Exp -> MAlt
simpleMA ps e = MAlt ps [Body [] e]

-- Match a multi-argument alternative
mmatchaltE :: (Fresh f) => [Sig] -> MAlt -> Exp -> f Exp
mmatchaltE args (MAlt ps b) n = do
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
      mkcases :: (Fresh m) => [(Pat, Exp)] -> [Body] -> Exp -> m Exp
      mkcases [] bs n = matchbodiesE bs n
      mkcases ((p, x):ps) y n = do
        body <- mkcases ps y n
        matchpatE x p body n
  mkcases (zip ps (map varE args)) b n

-- Match multiple multi-argument alternatives
mmatchaltsE :: (Fresh f) => [Sig] -> [MAlt] -> Exp -> f Exp
mmatchaltsE args [] n = return n
mmatchaltsE args (m:ms) n = do
    n' <- mmatchaltsE args ms n
    mmatchaltE args m n'

-- | Desugar a case expression.
mcaseE :: Exp -> [Alt] -> Exp
mcaseE x alts
 = runFreshPretty $ matchaltsE x alts (errorE "case no match")

clauseE :: [MAlt] -> Exp
clauseE ms = runFreshPretty $ clauseE' ms (errorE "case no match")

clauseE' :: (Fresh f) => [MAlt] -> Exp -> f Exp
clauseE' [MAlt ps e] n = do
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
  b <- mmatchaltE vars (MAlt ps' e) n
  return $ lamsE vars b

clauseE' ms@(MAlt ps _ : _) n = do
    vars <- mapM fresh [Sig (name $ "_p" ++ show i) UnknownT | i <- [1..(length ps)]]
    b <- mmatchaltsE vars ms n
    return $ lamsE vars b

-- | Lambda with pattern matching.
mlamE :: [Pat] -> Exp -> Exp
mlamE ps e = clauseE [simpleMA ps e]

-- | Let with pattern matching
mletE :: Pat -> Exp -> Exp -> Exp
mletE  p v e = mcaseE v [simpleA p e]

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

-- | List comprehension.
lcompE :: Exp -> [Guard] -> Exp
lcompE e [BoolG t] | t == trueE = listE [e]
lcompE e [q] = lcompE e [q, BoolG trueE]
lcompE e (BoolG b : qs) = ifE b (lcompE e qs) (listE [])
lcompE e (PatG p l : qs) = 
  let ok = clauseE [
            simpleMA [p] (lcompE e qs),
            simpleMA [WildP] (listE [])
           ]
  in appsE (varE (Sig (name "concatMap") UnknownT)) [ok, l]
lcompE e (LetG decls : qs) = mletsE decls (lcompE e qs)

normalB :: Exp -> Body
normalB = Body []

