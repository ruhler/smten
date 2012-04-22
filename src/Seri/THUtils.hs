
module Seri.THUtils (
    apply, applyC, arrowts, string, integer, appts, fixUnit, tyvarname,
    prefixed, flattenforall
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
arrowts (x:xs) = AppT (AppT ArrowT x) (arrowts xs)

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

tyvarname :: TyVarBndr -> Name
tyvarname (PlainTV v) = v
tyvarname (KindedTV v _) = v

-- Add a prefix to a name.
prefixed :: String -> Name -> Name
prefixed pre x = mkName $ pre ++ nameBase x

-- Collapse nested ForallT types to a single ForallT.
flattenforall :: Type -> Type
flattenforall (ForallT vs ctx t) =
    case (flattenforall t) of
        ForallT vs' ctx' t' -> ForallT (vs++vs') (ctx ++ ctx') t'
        t' -> ForallT vs ctx t'
flattenforall t = t


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

