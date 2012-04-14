
module Seri.THUtils (
    apply, applyC, arrowts, string, integer, appts, desugar, fixUnit,
    ) where 

import Data.Generics

import Data.List(nub)
import Language.Haskell.TH

apply :: Name -> [Exp] -> Exp
apply n exps = foldl AppE (VarE n) exps

applyC :: Name -> [Exp] -> Exp
applyC n exps = foldl AppE (ConE n) exps

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

integer :: Integer -> Exp
integer i = LitE (IntegerL i)

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


-- There seems to be a bug with quasi quoters where the type "GHC.Unit.()" is
-- interpreted as a data constructor instead of a type. To allow use of the
-- unit type in quasi quotes, we replace occurrences of the type constructor
-- "GHC.Unit.()" with the type constructor "()".
fixUnit :: (Data a) => a -> a
fixUnit = 
  let base :: Type -> Type
      base (ConT n) | nameBase n == "()" = ConT (mkName "()")
      base t = t
  in everywhere $ mkT base

