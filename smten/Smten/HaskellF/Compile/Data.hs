
{-# LANGUAGE FlexibleContexts #-}

module Smten.HaskellF.Compile.Data (
   hsData,
    ) where

import Control.Monad.Error
import qualified Language.Haskell.TH.Syntax as H
import Data.List(genericLength, genericReplicate)

import Smten.Name
import Smten.Type
import Smten.Dec
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Name
import Smten.HaskellF.Compile.Type
import Smten.HaskellF.Compile.Kind

hsData :: Name -> [TyVar] -> [Con] -> HF [H.Dec]
hsData n tyvars constrs = do
    dataD <- mkDataD n tyvars constrs
    smtenTD <- mkSmtenTD n tyvars
    casesD <- mapM (mkCaseD n tyvars) constrs
    consD <- mapM (mkConD n tyvars) constrs
    return $ concat ([dataD, smtenTD] : (casesD ++ consD))

-- data T__Foo a b ...
mkDataD :: Name -> [TyVar] -> [Con] -> HF H.Dec
mkDataD n tyvars constrs = do
  let mkknd (ArrowK a b) = foldl H.AppT H.ArrowT [mkknd a, mkknd b]
      mkknd _ = H.StarT

      mkty (TyVar nm k)
        | knum k == 0 = H.PlainTV (hsName nm)
        | otherwise = H.KindedTV (hsName nm) (mkknd k)

      tyvars' = map mkty tyvars
  return $ H.DataD [] (hsTyName n) tyvars' [] []

-- Note: we currently don't support crazy kinded instances of SmtenT. This
-- means we are limited to "linear" kinds of the form (* -> * -> ... -> *)
--
-- To handle that properly, we chop off as many type variables as needed to
-- get to a linear kind.
--   call the chopped off type variables c1, c2, ...
--
-- instance (SmtenN c1, SmtenN c2, ...) => SmtenTN (Foo c1 c2 ...) where
--    smtenTN _ = appsT (conT "Foo") [smtenT (undefined :: c1), smtenT (undefined :: c2), ...]
mkSmtenTD :: (MonadError String m) => Name -> [TyVar] -> m H.Dec
mkSmtenTD n tyvars = return $
  let (rkept, rdropped) = span (\(TyVar n k) -> knum k == 0) (reverse tyvars)
      nkept = genericLength rkept
      dropped = reverse rdropped
      ctx = [H.ClassP (nmk "Smten.Type.SmtenT" k) [H.VarT (hsName n)] | TyVar n k <- dropped]
      cont = H.AppE (H.VarE (H.mkName "Smten.Type.conT"))
                    (H.AppE (H.VarE (H.mkName "Smten.Name.name"))
                            (H.LitE (H.StringL (unname n))))

      mkarg :: TyVar -> H.Exp
      mkarg (TyVar n k) = 
        H.AppE (H.VarE (nmk "Smten.Type.smtenT" k))
               (H.SigE (H.VarE (H.mkName "Prelude.undefined"))
                       (foldl H.AppT (H.VarT (hsName n)) (genericReplicate (knum k) (H.ConT (H.mkName "()")))))

      args = H.ListE (map mkarg dropped)
      body = H.AppE (H.AppE (H.VarE (H.mkName "Smten.Type.appsT")) cont) args
      smtent = H.FunD (nmn "smtenT" nkept) [
                H.Clause [H.WildP] (H.NormalB body) []]
      tyt = H.AppT (H.ConT $ nmn "Smten.Type.SmtenT" nkept)
                   (foldl H.AppT (H.ConT (hsTyName n)) [H.VarT (hsName n) | TyVar n _ <- dropped])
  in H.InstanceD ctx tyt [smtent]
  
-- __caseFooB :: Foo -> (Function FooB1 (Function FooB2 ... z) -> z -> z
-- __caseFooB = caseHF "FooB"
mkCaseD :: Name -> [TyVar] -> Con -> HF [H.Dec]
mkCaseD n tyvars (Con cn tys) = do
  let dt = appsT (conT n) (map tyVarType tyvars)
      z = VarT (name "z") StarK
  ht <- hsTopFunctionType [] [dt, arrowsT (tys ++ [z]), z, z]
  let sigD = H.SigD (casenm cn) ht

      body = H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.caseHF"))
                    (H.LitE (H.StringL (unname cn)))
      clause = H.Clause [] (H.NormalB body) []
      funD = H.FunD (casenm cn) [clause]
  return [sigD, funD]

-- __mkFooB :: Function FooB1 (Function FooB2 ... Foo)
-- __mkFooB = lamHF "x1" $ \x1 ->
--              lamHF "x2" $ \x2 ->
--                ... -> conHF' "FooB" [unbox x1, unbox x2, ...]
mkConD :: Name -> [TyVar] -> Con -> HF [H.Dec]
mkConD n tyvars (Con cn tys) = do
  let dt = appsT (conT n) (map tyVarType tyvars)
      t = arrowsT $ tys ++ [dt]
  ht <- hsTopType [] t
  let sigD = H.SigD (connm cn) ht
      vars = ["x" ++ show i | i <- take (length tys) [1..]]

      mklam :: String -> H.Exp -> H.Exp
      mklam nm x = foldl1 H.AppE [
         H.VarE (H.mkName "Smten.HaskellF.HaskellF.lamHF"),
         H.LitE (H.StringL nm),
         H.LamE [H.VarP (H.mkName nm)] x]

      unbox = H.VarE (H.mkName "Smten.HaskellF.HaskellF.unbox")
      body = foldl1 H.AppE [
                H.VarE (H.mkName "Smten.HaskellF.HaskellF.conHF'"),
                H.LitE (H.StringL (unname cn)),
                H.ListE [H.AppE unbox (H.VarE (H.mkName nm)) | nm <- vars]]
      lams = foldr mklam body vars
      clause = H.Clause [] (H.NormalB lams) []
      funD = H.FunD (connm cn) [clause]
  return [sigD, funD]

