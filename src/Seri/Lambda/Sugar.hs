
module Seri.Lambda.Sugar (
    trueE, falseE,
    ifE, Stmt(..), doE, tupE, tupP, Clause(..), clauseE, lamE, listE,
    ) where

import Seri.Lambda.IR
import Seri.Lambda.Types

trueE :: Exp
trueE = ConE (Sig "True" (ConT "Bool"))

falseE :: Exp
falseE = ConE (Sig "False" (ConT "Bool"))

ifE :: Exp -> Exp -> Exp -> Exp
ifE p a b = CaseE p [Match (ConP (Sig "True" (ConT "Bool")) []) a,
                     Match (ConP (Sig "False" (ConT "Bool")) []) b]

data Stmt = 
    BindS Sig Exp
  | NoBindS Exp


-- De-sugar a do statement.
--  VariInfo is the instance of Monad the do statement is for.
doE :: VarInfo -> [Stmt] -> Exp
doE vi [NoBindS e] = e 
doE vi ((NoBindS e):stmts) =
    let rest = doE vi stmts
    in AppE (AppE (VarE (Sig ">>" (arrowsT [typeof e, typeof rest, typeof rest])) vi) e) rest
doE vi ((BindS s e):stmts) =
    let f = LamE s (doE vi stmts)
    in AppE (AppE (VarE (Sig ">>=" (arrowsT [typeof e, typeof f, outputT (typeof f)])) vi) e) f

tupE :: [Exp] -> Exp
tupE [x] = x
tupE es@(_:_:_) =
    let n = length es
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof es
        ttype = arrowsT (types ++ [foldl AppT (ConT name) types])
    in foldl AppE (ConE (Sig name ttype)) es

tupP :: [Pat] -> Pat
tupP [p] = p
tupP ps@(_:_:_) =
    let n = length ps
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof ps
        ttype = arrowsT (types ++ [foldl AppT (ConT name) types])
    in ConP (Sig name ttype) ps
    

data Clause = Clause [Pat] Exp
    
-- Given a set of function clauses, return a corresponding expression to
-- implement those clauses.
--  All clauses must have the same number of patterns.
clauseE :: [Clause] -> Exp
clauseE [Clause [] e] = e
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

-- Multi-arg lambda expressions.
--  Transforms \a b ... c -> e
--        into \a -> \b -> ... \c -> e
lamE :: [Sig] -> Exp -> Exp
lamE [] e = e
lamE (x:xs) e = LamE x (lamE xs e)

listE :: [Exp] -> Exp
listE [x] =
 let t = typeof x
 in AppE (AppE (ConE (Sig ":" (arrowsT [t, listT t, listT t]))) x)
         (ConE (Sig "[]" (listT t)))
listE (x:xs) = 
 let t = typeof x
 in AppE (AppE (ConE (Sig ":" (arrowsT [t, listT t, listT t]))) x) (listE xs)

