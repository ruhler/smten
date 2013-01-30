
-- | Syntactic sugar involving pattern matching.
module Seri.Exp.Match (
    Pat(..), Guard(..), Body(..), Alt(..), MAlt(..),
    tupleP, listP, charP, stringP, numberP, sigP,
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
         | SigP Pat Type
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

sigP :: Pat -> Type -> Pat
sigP = SigP

-- | Perform a pattern match.
-- case x of
--     p -> yv
--     _ -> n
patM :: (Fresh f) => Exp -> Pat -> Exp -> Exp -> f Exp
patM _ WildP yv _ = return yv
patM x (VarP n) yv _ = return $ appE (lamE (Sig n UnknownT) yv) x
patM x (AsP nm p) yv n = do
    rest <- patM x p yv n
    return $ letE (Sig nm (typeof x)) x rest
patM x (LitP e) yv n =
  let p = appsE (varE (Sig (name "==") UnknownT)) [e, x]
  in return $ ifE p yv n
patM x (SigP p t) yv n = patM (sigE x t) p yv n
patM x (ConP nm ps) yv n | isSimple n = do
      y <- clauseM [simpleMA ps yv] n
      return $ CaseE x (Sig nm UnknownT) y n
patM x p y n = do
  nv <- fresh (Sig (name "_n") UnknownT)
  body <- patM x p y (varE nv)
  return $ letE nv n body

data Guard = PatG Pat Exp
           | LetG [(Pat, Exp)]
           | BoolG Exp
    deriving (Eq, Show)

-- | Perform a guard match
--   | g = y
--   | otherwise = n
guardM :: (Fresh f) => Guard -> Exp -> Exp -> f Exp
guardM (PatG p x) y n = patM x p y n
guardM (LetG decls) y _ = return (mletsE decls y)
guardM (BoolG x) y n = return (ifE x y n)

-- | Perform multiple guard matches
--   | g1, g2, ... = y
--   | otherwise = n
guardsM :: (Fresh f) => [Guard] -> Exp -> Exp -> f Exp
guardsM [] y _ = return y 
guardsM (g:gs) y n = do
    y' <- guardsM gs y n
    guardM g y' n

data Body = Body [Guard] Exp
    deriving (Eq, Show)

bodyM :: (Fresh f) => Body -> Exp -> f Exp
bodyM (Body gs y) n = guardsM gs y n

bodiesM :: (Fresh f) => [Body] -> Exp -> f Exp
bodiesM [] n = return n
bodiesM (b:bs) n = do
    n' <- bodiesM bs n
    bodyM b n'

data Alt = Alt Pat [Body]
    deriving (Eq, Show)

simpleA :: Pat -> Exp -> Alt
simpleA p e = Alt p [Body [] e]

-- Match a single alternative:
--  case x of
--    alt 
--    _ -> n
altM :: (Fresh f) => Exp -> Alt -> Exp -> f Exp
altM x (Alt p bs) n = do
    body <- bodiesM bs n
    patM x p body n

-- Match multiple alternatives:
--   case x of
--     alt1 
--     alt2
--     ...
--     _ -> n
altsM :: (Fresh f) => Exp -> [Alt] -> Exp -> f Exp
altsM _ [] n = return n
altsM x (a:as) n | isSimple x = do
    n' <- altsM x as n
    altM x a n'
altsM x as n = do
    xv <- fresh (Sig (name "_x") UnknownT)
    body <- altsM (varE xv) as n
    return $ letE xv x body

data MAlt = MAlt [Pat] [Body]
    deriving (Eq, Show)

simpleMA :: [Pat] -> Exp -> MAlt
simpleMA ps e = MAlt ps [Body [] e]

-- Match a multi-argument alternative
maltM :: (Fresh f) => [Sig] -> MAlt -> Exp -> f Exp
maltM args (MAlt ps b) n = do
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
      mkcases [] bs n = bodiesM bs n
      mkcases ((p, x):ps) y n = do
        body <- mkcases ps y n
        patM x p body n
  mkcases (zip ps (map varE args)) b n

-- Match multiple multi-argument alternatives
maltsM :: (Fresh f) => [Sig] -> [MAlt] -> Exp -> f Exp
maltsM args [] n = return n
maltsM args (m:ms) n = do
    n' <- maltsM args ms n
    maltM args m n'

-- | Desugar a case expression.
mcaseE :: Exp -> [Alt] -> Exp
mcaseE x alts
 = runFreshPretty $ altsM x alts (errorE "case no match")

clauseE :: [MAlt] -> Exp
clauseE ms = runFreshPretty $ clauseM ms (errorE "case no match")

clauseM :: (Fresh f) => [MAlt] -> Exp -> f Exp
clauseM [MAlt ps e] n = do
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
  b <- maltM vars (MAlt ps' e) n
  return $ lamsE vars b

clauseM ms@(MAlt ps _ : _) n = do
    vars <- mapM fresh [Sig (name $ "_p" ++ show i) UnknownT | i <- [1..(length ps)]]
    b <- maltsM vars ms n
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

