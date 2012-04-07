
module Seri.THUtils (
    apply, arrowts, string, appts, desugar,
    ) where 

import Data.List(nub)
import Language.Haskell.TH

apply :: Name -> [Exp] -> Exp
apply n exps = foldl AppE (VarE n) exps

-- arrowts 
--  Turn a list of types [a, b, c, ...]
--  Into a type (a -> b -> c -> ...)
arrowts :: [Type] -> Type
arrowts [t] = t
arrowts (ta:tb:ts) = arrowts ((AppT (AppT ArrowT ta) tb) : ts)

-- appts
--  Turn a list of types [a, b, c, ...]
--  Into a type (a b c ...)
appts :: [Type] -> Type
appts ts = foldl1 AppT ts

string :: Name -> Exp
string n = LitE (StringL (nameBase n))

-- Return a list of all the variable type names in the given type.
tvarnames :: Type -> [Name]
tvarnames (ForallT _ _ t) = tvarnames t
tvarnames (VarT nm) = [nm]
tvarnames (AppT a b) = nub $ (tvarnames a) ++ (tvarnames b)
tvarnames t = []

-- Desugar a do block into bind and return calls.
desugar :: [Stmt] -> Exp
desugar [NoBindS e] = e
desugar ((NoBindS e):stmts)
    = AppE (AppE (VarE $ mkName ">>") e) (desugar stmts)
desugar ((BindS p e):stmts)
    = AppE (AppE (VarE $ mkName ">>=") e) (LamE [p] (desugar stmts))


