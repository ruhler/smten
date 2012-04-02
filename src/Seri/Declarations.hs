
{-# LANGUAGE TemplateHaskell #-}

module Seri.Declarations (
    name_P, name_C, name_D,
    declprim, declval, tvarnames
    ) where

import Data.List(nub)
import Language.Haskell.TH

import qualified Seri.IR as SIR
import qualified Seri.Typed as S

-- The name of the (possibly) polymorphic function generated.
name_P :: SIR.Name -> Name
name_P x = mkName $ "_seriP_" ++ x

-- The name of the concrete function generated.
name_C :: SIR.Name -> Name
name_C x = mkName $ "_seriC_" ++ x

-- The name of the context (Declarations) function generated.
name_D :: SIR.Name -> Name
name_D x = mkName $ "_seriD_" ++ x


declprim :: SIR.Name -> Name -> Q Type -> Q [Dec]
declprim nm prim ty = do
    t <- ty
    e <- [e| S.primitive $(conE prim) |]
    return $ declval nm t e []

-- declval name ty exp free
-- Make a seri value declaration.
--   name - name of the seri value being defined.
--   ty - the polymorphic haskell type of the expression.
--          For example: (SeriType a) => TypedExp (a -> Integer)
--   exp - the value
--   free - a list of unbounded variables used in exp which have already been
--          defined.
--
-- The following haskell declarations are generated:
--  _seriP_name - the expression with a polymorphic haskell type.
--  _seriC_name - the expression with a concrete haskell type.
--  _seriD_name - a list of the seri declarations needed to evaluate this
--                value.
declval :: SIR.Name -> Type -> Exp -> [SIR.Name] -> [Dec]
declval n t e free =
  let sig_P = SigD (name_P n) t
      impl_P = FunD (name_P n) [Clause [] (NormalB e) []]

      sig_C = SigD (name_C n) (concretize t)
      impl_C = FunD (name_C n) [Clause [] (NormalB (VarE (name_P n))) []]

      subctx = map (\fn -> VarE (name_D fn)) free
      mydecl = ListE [apply 'S.valD [LitE (StringL n), VarE (name_C n)]]
      concated = apply 'concat [ListE (mydecl:subctx)]
      nubbed = apply 'SIR.nubdecl [concated]

      sig_D = SigD (name_D n) (AppT ListT (ConT ''SIR.Dec))
      impl_D = FunD (name_D n) [Clause [] (NormalB nubbed) []]
    in [sig_P, impl_P, sig_C, impl_C, sig_D, impl_D]

declval' :: [Dec] -> [Dec]
declval' x = error $ "TODO: declval' " ++ show x

-- Given a potentially polymorphic haskell type, convert it to a concrete
-- haskell type which represents the polymorphic seri type.
--
-- In other words, replace all occurences of VarT "foo" with VarT_foo.
concretize :: Type -> Type
concretize (ForallT _ _ t) = concretize t
concretize (VarT nm) = ConT $ mkName ("VarT_" ++ (nameBase nm))
concretize (AppT a b) = AppT (concretize a) (concretize b)
concretize t = t

-- Return a list of all the variable type names in the given type.
tvarnames :: Type -> [Name]
tvarnames (ForallT _ _ t) = tvarnames t
tvarnames (VarT nm) = [nm]
tvarnames (AppT a b) = nub $ (tvarnames a) ++ (tvarnames b)
tvarnames t = []


apply :: Name -> [Exp] -> Exp
apply n exps = foldl AppE (VarE n) exps

