
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
    symbD <- mkSymbD n tyvars constrs
    casesD <- mapM (mkCaseD n tyvars) constrs
    consD <- mapM (mkConD n tyvars) constrs
    return $ concat ([dataD, smtenTD, symbD] : (casesD ++ consD))

-- data Foo a b ... =
--    FooA FooA1 FooA2 ...
--  | FooB FooB1 FooB2 ...
--    ...
--  | Foo__s ExpH
mkDataD :: Name -> [TyVar] -> [Con] -> HF H.Dec
mkDataD n tyvars constrs = do
  let tyvars' = map (H.PlainTV . hsName . tyVarName) tyvars
  constrs' <- mapM hsCon constrs
  let sconstr = H.NormalC (symnm n) [(H.NotStrict, H.ConT (H.mkName "Smten.ExpH.ExpH"))]
  return $ H.DataD [] (hsTyName n) tyvars' (constrs' ++ [sconstr]) []

hsCon :: Con -> HF H.Con
hsCon (Con n tys) = do
    ts <- mapM hsType tys
    return $ H.NormalC (hsName n) (map (\t -> (H.NotStrict, t)) ts)

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
  
-- instance HaskellFN Foo where
--  boxN ...
--  unboxN ...
--
-- Note: we do the same thing with crazy kinds as mkSmtenTD
mkSymbD :: (MonadError String m) => Name -> [TyVar] -> [Con] -> m H.Dec
mkSymbD n tyvars constrs = do
    let (rkept, rdropped) = span (\(TyVar n k) -> knum k == 0) (reverse tyvars)
        nkept = genericLength rkept
        dropped = reverse rdropped
    boxD <- mkBoxD n nkept constrs
    unboxD <- mkUnboxD n nkept constrs
    let ctx = [H.ClassP (nmk "Smten.HaskellF.HaskellF.HaskellF" k) [H.VarT (hsName n)] | TyVar n k <- dropped]
        clsname = nmn "Smten.HaskellF.HaskellF.HaskellF" nkept
        ty = H.AppT (H.ConT clsname) 
                    (foldl H.AppT (H.ConT (hsTyName n)) [H.VarT (hsName n) | TyVar n _ <- dropped])
    return $ H.InstanceD ctx ty [boxD, unboxD]

--  boxN e
--   | Just [a, b, ...] <- de_conHF "FooA" e = FooA (box a) (box b) ...
--   | Just [a, b, ...] <- de_conHF "FooB" e = FooB (box a) (box b) ...
--   ...
--   | otherwise = Foo__s e
mkBoxD :: (MonadError String m) => Name -> Integer -> [Con] -> m H.Dec
mkBoxD n bn constrs = do
  let mkGuard :: Con -> (H.Guard, H.Exp)
      mkGuard (Con cn tys) = 
        let argnms = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
            pat = H.ConP (H.mkName "Prelude.Just") [H.ListP (map H.VarP argnms)]
            src = foldl1 H.AppE [
                    H.VarE (H.mkName "Smten.HaskellF.HaskellF.de_conHF"),
                    H.LitE (H.StringL (unname cn)),
                    H.VarE (H.mkName "e")]
            guard = H.PatG [H.BindS pat src]
            boxes = [H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.box")) (H.VarE an) | an <- argnms]
            body = foldl H.AppE (H.ConE (hsName cn)) boxes
        in (guard, body)

      sguard = (H.NormalG (H.VarE (H.mkName "Prelude.otherwise")), H.AppE (H.ConE (symnm n)) (H.VarE (H.mkName "e")))
      guards = map mkGuard constrs ++ [sguard]
      clause = H.Clause [H.VarP (H.mkName "e")] (H.GuardedB guards) []
  return $ H.FunD (nmn "box" bn) [clause]
  

--  unboxN x
--   | FooA a b ... <- x = conHF x "FooA" [unbox a, unbox b, ...]
--   | FooB a b ... <- x = conHF x "FooB" [unbox a, unbox b, ...]
--   ...
--   | Foo__s v <- x = v
mkUnboxD :: (MonadError String m) => Name -> Integer -> [Con] -> m H.Dec
mkUnboxD n bn constrs = do
    let mkGuard :: Con -> (H.Guard, H.Exp)
        mkGuard (Con cn tys) =
          let argnms = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
              pat = H.ConP (hsName cn) (map H.VarP argnms)
              src = H.VarE (H.mkName "x")
              guard = H.PatG [H.BindS pat src]
              unboxes = [H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.unbox")) (H.VarE an) | an <- argnms]
              body = foldl1 H.AppE [
                        H.VarE (H.mkName "Smten.HaskellF.HaskellF.conHF"),
                        H.VarE (H.mkName "x"),
                        H.LitE (H.StringL (unname cn)),
                        H.ListE unboxes]
          in (guard, body)

        spat = H.ConP (symnm n) [H.VarP (H.mkName "v")]
        ssrc = H.VarE (H.mkName "x")
        sguard = (H.PatG [H.BindS spat ssrc], H.VarE (H.mkName "v"))
        guards = map mkGuard constrs ++ [sguard]
        clause = H.Clause [H.VarP (H.mkName "x")] (H.GuardedB guards) []
    return $ H.FunD (nmn "unbox" bn) [clause]

-- __caseFooB :: Foo -> (Function FooB1 (Function FooB2 ... z) -> z -> z
-- __caseFooB = \x y n -> case x of
--                           FooB a b ... -> applyHF (applyHF y a) b ...
--                           Foo__s _ -> caseHF "FooB" x y n
--                           _ -> n
mkCaseD :: Name -> [TyVar] -> Con -> HF [H.Dec]
mkCaseD n tyvars (Con cn tys) = do
  -- Note: we use this funny arrow hack to be able to reuse hsTopType to
  -- generate the context for us, but while using haskell -> instead of the HF
  -- function type constructor.
  -- TODO: surely there must be a nicer way to do this?
  let dt = appsT (conT n) (map tyVarType tyvars)
      z = VarT (name "z") StarK
      arrT = \a b -> appsT (conT (name "HS_ARROW_HACK")) [a, b]
      arrsT = foldr1 arrT
      t = arrsT [dt, arrowsT (tys ++ [z]), z, z]
  ht' <- hsTopType [] t
  let arrowhack :: H.Type -> H.Type
      arrowhack (H.ForallT vars ctx t) = H.ForallT vars ctx (arrowhack t)
      arrowhack t@(H.ConT n)
        | H.nameBase n == "HS_ARROW_HACK" = H.ArrowT
        | otherwise = t
      arrowhack t@(H.VarT {}) = t
      arrowhack (H.AppT a b) = H.AppT (arrowhack a) (arrowhack b)

      ht = arrowhack ht'

      sigD = H.SigD (casenm cn) ht

      xsrc = H.VarE (H.mkName "x")

      yargs = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
      ypat = H.ConP (hsName cn) (map H.VarP yargs)
      apphf = H.VarE (H.mkName "Smten.HaskellF.HaskellF.applyHF")
      app = \a b -> foldl1 H.AppE [apphf, a, b]
      ybody = foldl app (H.VarE (H.mkName "y")) [H.VarE an | an <- yargs]
      ymatch = H.Match ypat (H.NormalB ybody) []

      spat = H.ConP (symnm n) [H.WildP]
      sbody = foldl1 H.AppE [
                H.VarE (H.mkName "Smten.HaskellF.HaskellF.caseHF"),
                H.LitE (H.StringL (unname cn)),
                H.VarE (H.mkName "x"),
                H.VarE (H.mkName "y"),
                H.VarE (H.mkName "n")]
      smatch = H.Match spat (H.NormalB sbody) []

      npat = H.WildP
      nbody = H.VarE (H.mkName "n")
      nmatch = H.Match npat (H.NormalB nbody) []

      thecase = H.CaseE xsrc [ymatch, smatch, nmatch]

      lams = H.LamE [H.VarP (H.mkName n) | n <- ["x", "y", "n"]] $ thecase
      clause = H.Clause [] (H.NormalB lams) []
      funD = H.FunD (casenm cn) [clause]
  return [sigD, funD]

-- __mkFooB :: Function FooB1 (Function FooB2 ... Foo)
-- __mkFooB = lamHF "x1" $ \x1 ->
--              lamHF "x2" $ \x2 ->
--                ... -> FooB x1 x2 ...
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

      body = foldl H.AppE (H.ConE (hsName cn)) [H.VarE (H.mkName nm) | nm <- vars]
      lams = foldr mklam body vars
      clause = H.Clause [] (H.NormalB lams) []
      funD = H.FunD (connm cn) [clause]
  return [sigD, funD]

