
{-# LANGUAGE PatternGuards #-}

-- | Syntactic sugar involving pattern matching.
module Smten.Exp.Match (
    Pat(..), Guard(..), Body(..), Alt(..), MAlt(..), WBodies(..),
    tupleP, listP, charP, stringP, numberP, sigP,
    mcaseE, clauseE, mlamE, mletE, mletsE,
    lcompE, normalB,
    simpleA, simpleMA,
    ) where

import Data.Functor((<$>))
import Data.Maybe(fromMaybe)

import Smten.Location
import Smten.Lit
import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Fresh
import Smten.Exp.Exp
import Smten.Exp.Utils
import Smten.Exp.Sugar

data Pat = ConP Name [Pat]
         | VarP Name
         | AsP Name Pat
         | LitP Exp
         | WildP
         | SigP Pat Type
    deriving (Eq, Show)

listP :: [Pat] -> Pat
listP [] = ConP nilN []
listP (x:xs) = ConP consN [x, listP xs]

charP :: Location -> Char -> Pat
charP l = LitP . litE l . charL

stringP :: Location -> String -> Pat
stringP l = listP . map (charP l)

numberP :: Location -> Integer -> Pat
numberP l = LitP . numberE l

tupleP :: [Pat] -> Pat
tupleP ps = ConP (tupleN (length ps)) ps

sigP :: Pat -> Type -> Pat
sigP = SigP

-- Share the given expression properly.
sharedM :: Location -> Exp -> (Exp -> Fresh Exp) -> Fresh Exp
sharedM _ x f | isSimple x = f x
sharedM l x f = do
   xv@(Sig nv _) <- fresh (Sig (name "_s") UnknownT)
   body <- f (varE l xv)
   let z = if isfree nv body
             then letE l xv x body 
             else body
   return z

-- | Perform a pattern match.
-- case x of
--     p -> yv
--     _ -> n
patM :: Location -> Exp -> Pat -> Exp -> Exp -> Fresh Exp
patM _ _ WildP yv _ = return yv
patM l x (VarP n) yv _ = return $ appE l (lamE l (Sig n UnknownT) yv) x
patM l x (AsP nm p) yv n = do
    rest <- patM l x p yv n
    return $ letE l (Sig nm (typeof x)) x rest
patM l x (LitP e) yv n =
  let p = appsE l (varE l (Sig (name "==") UnknownT)) [e, x]
  in return $ ifE l p yv n
patM l x (SigP p t) yv n = patM l (sigE l x t) p yv n
patM l x (ConP nm ps) yv n = sharedM l n $ \nv -> do
      y <- clauseM l [simpleMA l ps yv []] nv
      return $ CaseE l x (Sig nm UnknownT) y nv

data Guard = PatG Pat Exp
           | LetG [(Pat, Exp)]
           | BoolG Exp
    deriving (Eq, Show)

-- | Perform a guard match
--   | g = y
--   | otherwise = n
guardM :: Location -> Guard -> Exp -> Exp -> Fresh Exp
guardM l (PatG p x) y n = patM l x p y n
guardM l (LetG decls) y _ = return (mletsE l decls y)
guardM l (BoolG x) y n = return (ifE l x y n)

-- | Perform multiple guard matches
--   | g1, g2, ... = y
--   | otherwise = n
guardsM :: Location -> [Guard] -> Exp -> Exp -> Fresh Exp
guardsM _ [] y _ = return y 
guardsM l (g:gs) y n = sharedM l n $ \nv -> do
    y' <- guardsM l gs y nv
    guardM l g y' nv

data Body = Body Location [Guard] Exp
    deriving (Eq, Show)

bodyM :: Body -> Exp -> Fresh Exp
bodyM (Body l gs y) n = guardsM l gs y n

bodiesM :: [Body] -> Exp -> Fresh Exp
bodiesM [] n = return n
bodiesM (b:bs) n = do
    n' <- bodiesM bs n
    bodyM b n'

-- Bodies with a where clause
data WBodies = WBodies Location [Body] [(Pat, Exp)]
    deriving (Eq, Show)

wbodiesM :: WBodies -> Exp -> Fresh Exp
wbodiesM (WBodies l bs ls) n = mletsE l ls <$> bodiesM bs n

data Alt = Alt Pat WBodies
    deriving (Eq, Show)

simpleA :: Location -> Pat -> Exp -> [(Pat, Exp)] -> Alt
simpleA l p e ls = Alt p (WBodies l [Body l [] e] ls)

-- Match a single alternative:
--  case x of
--    alt 
--    _ -> n
altM :: Location -> Exp -> Alt -> Exp -> Fresh Exp
altM l x (Alt p bs) n = sharedM l n $ \nv -> do
    body <- wbodiesM bs nv
    patM l x p body nv

-- Match multiple alternatives:
--   case x of
--     alt1 
--     alt2
--     ...
--     _ -> n
altsM :: Location -> Exp -> [Alt] -> Exp -> Fresh Exp
altsM _ _ [] n = return n
altsM l x (a:as) n = sharedM l x $ \xv -> do
    n' <- altsM l xv as n
    altM l xv a n'

data MAlt = MAlt [Pat] WBodies
    deriving (Eq, Show)

simpleMA :: Location -> [Pat] -> Exp -> [(Pat, Exp)] -> MAlt
simpleMA l ps e ls = MAlt ps (WBodies l [Body l [] e] ls)

-- Match a multi-argument alternative
maltM :: Location -> [Sig] -> MAlt -> Exp -> Fresh Exp
maltM l args (MAlt ps b) n = do
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
      mkcases :: [(Pat, Exp)] -> WBodies -> Exp -> Fresh Exp
      mkcases [] bs n = wbodiesM bs n
      mkcases ((p, x):ps) y n = sharedM l n $ \nv -> do
        body <- mkcases ps y nv
        patM l x p body nv
  mkcases (zip ps (map (varE l) args)) b n

-- Match multiple multi-argument alternatives
maltsM :: Location -> [Sig] -> [MAlt] -> Exp -> Fresh Exp
maltsM _ args [] n = return n
maltsM l args (m:ms) n = do
    n' <- maltsM l args ms n
    maltM l args m n'

-- | Desugar a case expression.
mcaseE :: Location -> Exp -> [Alt] -> Exp
mcaseE l x alts
 = runFresh $ altsM l x alts (errorE l $ lmsg l "unhandled case")

clauseE :: Location -> [MAlt] -> Exp
clauseE l ms = runFresh $ clauseM l ms (errorE l $ lmsg l "unhandled case")

clauseM :: Location -> [MAlt] -> Exp -> Fresh Exp
clauseM l [MAlt ps e] n = do
  -- If we are only making one match, we pick the variables for the lambda
  -- more wisely to avoid silly things like:
  --    \_p1 -> let a = _p1
  --            in foo a
  -- This doesn't work if there are multiple matches because there are scoping
  -- issues then.
  let mkvar :: Pat -> Fresh (Sig, Pat)
      mkvar (VarP n) = return (Sig n UnknownT, WildP)
      mkvar (AsP n p) = return (Sig n UnknownT, p)
      mkvar p = do  
        s <- fresh $ Sig (name "_p") UnknownT
        return (s, p)
  pvs <- mapM mkvar ps
  let (vars, ps') = unzip pvs
  b <- maltM l vars (MAlt ps' e) n
  return $ lamsE l vars b

clauseM l ms@(MAlt ps _ : _) n = do
    vars <- mapM fresh [Sig (name $ "_p" ++ show i) UnknownT | i <- [1..(length ps)]]
    b <- maltsM l vars ms n
    return $ lamsE l vars b

-- | Lambda with pattern matching.
mlamE :: Location -> [Pat] -> Exp -> Exp
mlamE l ps e = clauseE l [simpleMA l ps e []]

-- | Let with pattern matching
mletE :: Location -> Pat -> Exp -> Exp -> Exp
mletE l p v e = mcaseE l v [simpleA l p e []]

-- | Sequential let with pattern matching
mletsE :: Location -> [(Pat, Exp)] -> Exp -> Exp
mletsE _ [] x = x
mletsE l ((p, v):ps) x = mletE l p v (mletsE l ps x)

-- Return true if the expression is simple.
-- If an expression is simple, there's no cost to duplicating it.
isSimple :: Exp -> Bool
isSimple (AppE {}) = False
isSimple (LamE {}) = False
isSimple (CaseE {}) = False
isSimple _ = True

-- | List comprehension.
lcompE :: Location -> Exp -> [Guard] -> Exp
lcompE l e [BoolG t] | Just True <- de_boolE t = listE l [e]
lcompE l e [q] = lcompE l e [q, BoolG (trueE l)]
lcompE l e (BoolG b : qs) = ifE l b (lcompE l e qs) (listE l [])
lcompE l e (PatG p v : qs) = 
  let ok = clauseE l [
            simpleMA l [p] (lcompE l e qs) [],
            simpleMA l [WildP] (listE l []) []
           ]
  in appsE l (varE l (Sig (name "concatMap") UnknownT)) [ok, v]
lcompE l e (LetG decls : qs) = mletsE l decls (lcompE l e qs)

normalB :: Location -> Exp -> Body
normalB l = Body l []

