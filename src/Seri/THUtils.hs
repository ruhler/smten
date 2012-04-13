
module Seri.THUtils (
    apply, arrowts, string, integer, appts, desugar, FixUnit(..),
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
-- unit type in quasi quotes, we replace occurrences of "GHC.Unit.()" with
-- just "()".
class FixUnit a where
    fixUnit :: a -> a

instance FixUnit Dec where
    fixUnit (SigD n t) = SigD n (fixUnit t)
    fixUnit (DataD ctx n vrs cons dervs)
        = DataD ctx n vrs (map fixUnit cons) dervs
    fixUnit d = d

instance FixUnit Con where
    fixUnit (NormalC n sts) = NormalC n (map (\(s, t) -> (s, fixUnit t)) sts)
    fixUnit (RecC n sts) = RecC n (map (\(fn, s, t) -> (fn, s, fixUnit t)) sts)

instance FixUnit Type where
    fixUnit (ForallT vs ctx t) = ForallT vs ctx (fixUnit t)
    fixUnit (ConT n) | nameBase n == "()" = ConT (mkName "()")
    fixUnit (AppT a b) = AppT (fixUnit a) (fixUnit b)
    fixUnit (SigT t k) = SigT (fixUnit t) k
    fixUnit t = t

