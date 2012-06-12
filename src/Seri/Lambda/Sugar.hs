
module Seri.Lambda.Sugar (
    ifE, Stmt(..), doE, tupE, tupP,
    ) where

import Seri.Lambda.IR
import Seri.Lambda.Typeof
import Seri.Lambda.Utils

ifE :: Exp -> Exp -> Exp -> Exp
ifE p a b = CaseE p [Match (ConP (Sig "True" (ConT "Bool")) []) a,
                     Match (ConP (Sig "False" (ConT "Bool")) []) b]

data Stmt = 
    BindS Sig Exp
  | NoBindS Exp


-- De-sugar a do statement.
--  Class is the instance of Monad the do statement is for.
doE :: Class -> [Stmt] -> Exp
doE cls [NoBindS e] = e 
doE cls ((NoBindS e):stmts) =
    let rest = doE cls stmts
    in AppE (AppE (VarE (Sig ">>" (arrowsT [typeof e, typeof rest, typeof rest]))
                        (Instance cls)) e) rest
doE cls ((BindS s e):stmts) =
    let f = LamE s (doE cls stmts)
    in AppE (AppE (VarE (Sig ">>=" (arrowsT [typeof e, typeof f, outputT (typeof f)]))
                        (Instance cls)) e) f

tupE :: [Exp] -> Exp
tupE es@(_:_:_) =
    let n = length es
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof es
        ttype = arrowsT (types ++ [foldl AppT (ConT name) types])
    in foldl AppE (ConE (Sig name ttype)) es

tupP :: [Pat] -> Pat
tupP ps@(_:_:_) =
    let n = length ps
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof ps
        ttype = arrowsT (types ++ [foldl AppT (ConT name) types])
    in ConP (Sig name ttype) ps
    
