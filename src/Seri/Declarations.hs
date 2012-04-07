
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Declarations (
    name_P, name_C, name_D,
    declprim, declval', declval, decltype, tvarnames
    ) where

import Data.List(nub)
import Language.Haskell.TH

import Seri.THUtils
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


declprim :: SIR.Name -> Q Type -> Q [Dec]
declprim nm ty = declval nm ty [e| S.primitive $(litE (StringL nm)) |] []

declval :: SIR.Name -> Q Type -> Q Exp -> [SIR.Name] -> Q [Dec]
declval n qt qe ns = do
    t <- qt
    e <- qe
    return $ declval' n t e ns

-- declval' name ty exp free
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
declval' :: SIR.Name -> Type -> Exp -> [SIR.Name] -> [Dec]
declval' n t e free =
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

-- decltype name
-- Given the name of a haskell type, make a seri type declaration for it.
--
-- The following is generated for the given type.
--  - an instance of SeriType.
--  - _seriP_Foo and friends for each constructor Foo
decltype :: Name -> Q [Dec]
decltype nm =
 let mkcon :: Type -> [TyVarBndr] -> Con -> [Dec]
     mkcon dt vars (NormalC nc sts) =
        let ts = map snd sts
            t = AppT (AppT (ConT ''S.Typed) (ConT ''SIR.Exp)) (arrowts (ts ++ [appts $ dt:(map (\(PlainTV n) -> VarT n) vars)]))
            ctx = map (\(PlainTV x) -> ClassP ''S.SeriType [VarT x]) vars
            ty = if null vars
                    then t
                    else ForallT vars ctx t
            e = apply 'S.conE [string nc]
        in declval' (nameBase nc) ty e []

 in do TyConI (DataD [] _ vars cs _) <- reify nm
       let numvars = length vars
       let classname = "SeriType" ++ if numvars == 0 then "" else show numvars
       let methname = "seritype" ++ if numvars == 0 then "" else show numvars
       let dec = FunD (mkName methname) [Clause [WildP] (NormalB (AppE (ConE 'SIR.ConT) (string nm))) []]
       let inst = InstanceD [] (AppT (ConT (mkName classname)) (ConT nm)) [dec]
       let cones = concat $ map (mkcon (ConT nm) vars) cs
       return $ concat [[inst], cones]

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


