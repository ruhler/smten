
-- | Constructor functions for desugaring higher level constructs into the
-- core Seri IR.
module Seri.Lambda.Sugar (
    ifE, lamE, appsE,
    Stmt(..), doE,
    Clause(..), clauseE,
    trueE, falseE, listE, tupE, tupP,
    ) where

import Seri.Lambda.IR
import Seri.Lambda.Types

-- | True
trueE :: Exp
trueE = ConE (Sig "True" (ConT "Bool"))

-- | False
falseE :: Exp
falseE = ConE (Sig "False" (ConT "Bool"))

-- | if p then a else b
ifE :: Exp -> Exp -> Exp -> Exp
ifE p a b = CaseE p [Match (ConP (Sig "True" (ConT "Bool")) []) a,
                     Match (ConP (Sig "False" (ConT "Bool")) []) b]

data Stmt = 
    BindS Sig Exp   -- ^ n <- e
  | NoBindS Exp     -- ^ e

-- | do { stmts }
-- VarInfo specifies which instance of Monad the 'do' is for.
-- The final statement of the 'do' must be a NoBindS.
doE :: VarInfo -> [Stmt] -> Exp
doE _ [] = error $ "doE on empty list"
doE vi [NoBindS e] = e 
doE vi ((NoBindS e):stmts) =
    let rest = doE vi stmts
        tbind = (arrowsT [typeof e, typeof rest, typeof rest])
    in appsE [VarE (Sig ">>" tbind) vi, e, rest]
doE vi ((BindS s e):stmts) =
    let f = LamE s (doE vi stmts)
        tbind = (arrowsT [typeof e, typeof f, outputT (typeof f)])
    in appsE [VarE (Sig ">>=" tbind) vi, e, f]

-- | (a, b, ... )
-- There must be at least one expression given.
--
-- If exactly one expression is given, that expression is returned without
-- tupling.
tupE :: [Exp] -> Exp
tupE [] = error $ "tupE on empty list"
tupE [x] = x
tupE es@(_:_:_) =
    let n = length es
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof es
        ttype = arrowsT (types ++ [foldl AppT (ConT name) types])
    in foldl AppE (ConE (Sig name ttype)) es

-- | (a, b, ... )
-- There must be at least one pattern given.
--
-- If exactly one pattern is given, that pattern is returned without
-- tupling.
tupP :: [Pat] -> Pat
tupP [] = error $ "tupP on empty list"
tupP [p] = p
tupP ps@(_:_:_) =
    let n = length ps
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof ps
        ttype = arrowsT (types ++ [foldl AppT (ConT name) types])
    in ConP (Sig name ttype) ps
    

data Clause = Clause [Pat] Exp
    
-- | Given a set of function clauses, return a corresponding expression to
-- implement those clauses.
--
-- All clauses must have the same number of patterns. The number of patterns
-- may be 0. At least one clause must be given.
clauseE :: [Clause] -> Exp
clauseE [] = error $ "clauseE on empty list"
clauseE ((Clause [] e):_) = e
clauseE clauses@(_:_) = 
  let Clause pats1 _ = head clauses
      nargs = length pats1
    
      mkmatch :: Clause -> Match
      mkmatch (Clause pats body) = Match (tupP pats) body

      args = [[c] | c <- take nargs "abcdefghijklmnopqrstuvwxyz"]
      casearg = tupE [VarE (Sig n (typeof p)) Bound | (n, p) <- zip args pats1]
      caseexp = CaseE casearg (map mkmatch clauses)
      lamargs = [Sig n (typeof p) | (n, p) <- zip args pats1]
      
  in lamE lamargs caseexp

-- | \a b ... c -> e
lamE :: [Sig] -> Exp -> Exp
lamE [] e = e
lamE (x:xs) e = LamE x (lamE xs e)

-- | (a b ... c)
appsE :: [Exp] -> Exp
appsE = foldl1 AppE

-- | [a, b, ..., c]
-- The list must be non-null so the type can properly be inferred.
listE :: [Exp] -> Exp
listE [x] =
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE [ConE (Sig ":" consT), x, ConE (Sig "[]" (listT t))]
listE (x:xs) = 
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE [ConE (Sig ":" consT), x, listE xs]

