
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.TyCon (
    tyconCG,    
  ) where

import Class
import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type
import qualified Smten.Plugin.Output.Syntax as S

-- Declare a type constructor.
-- This works for both normal type constructors and type class constructors.
tyconCG :: TyCon -> CG [S.Dec]
tyconCG t
 | Just cls <- tyConClass_maybe t
 , Just [dc] <- tyConDataCons_maybe t = do
     let mkfield :: Id -> CG S.RecField
         mkfield x = do
           let (vs, mt) = splitForAllTys $ varType x
               vs' = filter (flip notElem (tyConTyVars t)) vs
               mt' = snd $ splitFunTy mt
           t <- topTypeCG $ mkForAllTys vs' mt'
           nm <- nameCG $ varName x
           return $ S.RecField nm t
     fields <- mapM mkfield (classAllSelIds cls)
     cn <- nameCG $ dataConName dc
     t' <- nameCG $ tyConName t
     vs <- mapM (qnameCG . varName) (tyConTyVars t)
     return [S.DataD (S.Data t' vs [S.RecC cn fields])]
        
 | Just cs <- tyConDataCons_maybe t = dataCG (tyConName t) (tyConTyVars t) cs
 | isSynTyCon t = return []
 | otherwise = do
      lift $ errorMsg (text "Unsupported TyCon in tyconCG: " <+> ppr t)
      return []

dataCG :: Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
dataCG n tyvars constrs = do
    dataD <- mkDataD n tyvars constrs
    shsD <- smtenHS n tyvars constrs
    --itehelpers <- mkIteHelpersD n tyvars constrs
    --return $ concat [dataD, itehelpers, shsD]
    return $ concat [dataD, shsD]
  
-- data Foo a b ... = FooA A1 A2 ...
--                  | FooB B1 B2 ...
--                  ...
--                  | FooK K1 K2 ...
--                  | Foo_Prim (Model -> Foo a b ...) (Foo a b ...)
--                  | Foo_Error ErrorString
--                  | Foo_Ite {
--                      __iteFooA :: Prelude.Maybe (BoolF, Foo a b ...),
--                      __iteFooB :: Prelude.Maybe (BoolF, Foo a b ...),
--                      ...,
--                      __iteFoo_Error :: Prelude.Maybe (BoolF, Foo a b ...)
--                    }
mkDataD :: Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkDataD nm tyvars constrs = do
  let mkcon :: DataCon -> CG S.Con
      mkcon d = do
        nm <- nameCG (dataConName d)
        tys <- mapM typeCG (dataConOrigArgTys d)
        return $ S.Con nm tys

  nm' <- nameCG nm
  vs <- mapM (qnameCG . varName) tyvars
  ks <- mapM mkcon constrs

  tyvs' <- mapM tyvarCG tyvars

  qtyname <- qtynameCG nm
  primnm <- primnmCG nm
  errnm <- errnmCG nm
  iteerrnm <- iteerrnmCG nm
  iteflnms <- mapM (iteflnmCG . dataConName) constrs
  itenm <- itenmCG nm
  let tyme = S.ConAppT qtyname tyvs'
      asn = S.arrowT (S.ConAppT "Smten.Runtime.Model.Model" []) tyme
      prim = S.Con primnm [asn, tyme]

      err = S.Con errnm [S.ConAppT "Smten.Runtime.ErrorString.ErrorString" []]

      tybool = S.ConAppT "Smten.Runtime.Formula.BoolF" []
      tyfield = S.ConAppT "Prelude.Maybe" [S.ConAppT "(,)" [tybool, tyme]]

      iteerr = S.RecField iteerrnm tyfield
      ites = [S.RecField iteflnm tyfield | iteflnm <- iteflnms]
      ite = S.RecC itenm (ites ++ [iteerr])

      allks = ks ++ [prim, err, ite]
  addimport "Smten.Runtime.ErrorString"
  addimport "Smten.Runtime.Formula"
  addimport "Smten.Runtime.Model"
  addimport "Prelude"
  return [S.DataD (S.Data nm' vs allks)]

-- Note: we currently don't support crazy kinded instances of SmtenHS. This
-- means we are limited to "linear" kinds of the form (* -> * -> ... -> *)
--
-- To handle that properly, we chop off as many type variables as needed to
-- get to a linear kind.
--   call the chopped off type variables c1, c2, ...
--
-- instance (SmtenN c1, SmtenN c2, ...) => SmtenHSN (Foo c1 c2 ...) where
--   realizeN = ...
--   primitiveN = ...
--   errorN = ...
--   ...
smtenHS :: Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
smtenHS nm tyvs cs = do
   let (rkept, rdropped) = span ((==) 0 . knum . tyVarKind) (reverse tyvs)
       n = length rkept
       dropped = reverse rdropped
   tyvs' <- mapM tyvarCG dropped
   qtyname <- qtynameCG nm
   let ctx = [S.ConAppT ("Smten.Runtime.SmtenHS.SmtenHS" ++ show (knum (tyVarKind v))) [tyv] | (v, tyv) <- zip dropped tyvs']
       ty = S.ConAppT ("Smten.Runtime.SmtenHS.SmtenHS" ++ show n) [S.ConAppT qtyname tyvs']
   addimport "Smten.Runtime.SmtenHS"
   rel <- realizeD nm n cs
   --prim <- primD nm n
   --ite <- iteD nm n cs
   --err <- errorD nm n
   --sapp <- sappD nm n cs
   --return [S.InstD ctx ty [rel, ite, prim, err, sapp]]
   return [S.InstD ctx ty [rel]]

--   primN = Foo_Prim
primD :: Name -> Int -> CG S.Method
primD nm n = do
  qprimnm <- qprimnmCG nm
  return $ S.Method ("primitive" ++ show n) (S.VarE qprimnm)

---- iteN p (FooA a1 a2 ...) (FooA b1 b2 ...) = FooA (ite p a1 b1) (ite p a2 b2) ...
---- iteN p (FooB a1 a2 ...) (FooB b1 b2 ...) = FooB (ite p a1 b1) (ite p a2 b2) ...
----  ...
---- iteN p (Foo_Error a1) (Foo_Error b1) = Foo_Error (ite p a1 b1)
---- iteN p a@(Foo_Ite {}) b@(Foo_ite {}) = Foo_Ite {
----           __iteFooA = flmerge p (__iteFooA a) (__iteFooA b)
----           __iteFooB = flmerge p (__iteFooB a) (__iteFooB b)
----           ...
----         }
---- iteN p a@(Foo_Prim r c) b -> Foo_Prim (iterealize p a b) (ite p c b)
---- iteN p a b@(Foo_Prim r c) -> Foo_Prim (iterealize p a b) (ite p a c)
---- iteN p a b -> ite p (__LiftIteFoo a) (__LiftIteFoo b)
--iteD :: Name -> Int -> [Con] -> CG H.Dec
--iteD n k cs = do
--  let ite a b = foldl1 H.AppE [
--                  H.VarE $ H.mkName "Smten.ite",
--                  H.VarE $ H.mkName "p", a, b]
--
--      mkcon :: Con -> H.Clause
--      mkcon (Con cn cts) =
--        let as = [H.mkName $ "a" ++ show i | i <- [1..length cts]]
--            bs = [H.mkName $ "b" ++ show i | i <- [1..length cts]]
--            pats = [H.VarP $ H.mkName "p",
--                    H.ConP (qnameCG cn) (map H.VarP as),
--                    H.ConP (qnameCG cn) (map H.VarP bs)]
--            ites = [ite (H.VarE a) (H.VarE b) | (a, b) <- zip as bs]
--            body = foldl H.AppE (H.ConE $ qnameCG cn) ites
--        in H.Clause pats (H.NormalB body) []
--
--      an = H.mkName "a"
--      bn = H.mkName "b"
--      ap = H.VarP an
--      bp = H.VarP bn
--      ae = H.VarE an
--      be = H.VarE bn
--      errpats = [H.VarP $ H.mkName "p",
--                 H.ConP (qerrnmCG n) [ap],
--                 H.ConP (qerrnmCG n) [bp]]
--      errbody = H.AppE (H.ConE (qerrnmCG n)) (ite ae be)
--      errcon = H.Clause errpats (H.NormalB errbody) []
--
--      mkfe :: Con -> H.FieldExp
--      mkfe (Con cn tys) = (iteflnmCG cn, foldl1 H.AppE [
--                        H.VarE $ H.mkName "Smten.flmerge",
--                        H.VarE $ H.mkName "p",
--                        H.AppE (H.VarE $ qiteflnmCG cn) ae,
--                        H.AppE (H.VarE $ qiteflnmCG cn) be])
--
--      efe = (iteerrnmCG n, foldl1 H.AppE [
--                        H.VarE $ H.mkName "Smten.flmerge",
--                        H.VarE $ H.mkName "p",
--                        H.AppE (H.VarE $ qiteerrnmCG n) ae,
--                        H.AppE (H.VarE $ qiteerrnmCG n) be])
--      itepats = [H.VarP $ H.mkName "p",
--                 H.AsP an (H.RecP (qitenmCG n) []),
--                 H.AsP bn (H.RecP (qitenmCG n) [])]
--      itebody = H.RecConE (qitenmCG n) (map mkfe cs ++ [efe])
--      itecon = H.Clause itepats (H.NormalB itebody) []
--
--      lprimpats = [H.VarP $ H.mkName "p",
--                   H.AsP an (H.ConP (qprimnmCG n) [H.VarP (H.mkName "r"), H.VarP $ H.mkName "c"]),
--                   bp]
--      lprimbody = foldl1 H.AppE [
--         H.ConE $ qprimnmCG n,
--         foldl1 H.AppE (map H.VarE [H.mkName $ "Smten.iterealize", H.mkName "p", an, bn]),
--         ite (H.VarE $ H.mkName "c") be]
--      lprimcon = H.Clause lprimpats (H.NormalB lprimbody) []
--
--      rprimpats = [H.VarP $ H.mkName "p", ap,
--                   H.AsP bn (H.ConP (qprimnmCG n) [H.VarP (H.mkName "r"), H.VarP $ H.mkName "c"])]
--      rprimbody = foldl1 H.AppE [
--         H.ConE $ qprimnmCG n,
--         foldl1 H.AppE (map H.VarE [H.mkName $ "Smten.iterealize", H.mkName "p", an, bn]),
--         ite ae (H.VarE $ H.mkName "c")]
--      rprimcon = H.Clause rprimpats (H.NormalB rprimbody) []
--
--      defpats = [H.VarP $ H.mkName "p", ap, bp]
--      defbody = ite (H.AppE (H.VarE $ qliftitenmCG n) ae)
--                    (H.AppE (H.VarE $ qliftitenmCG n) be)
--      defcon = H.Clause defpats (H.NormalB defbody) []
--
--  return $ H.FunD (H.mkName $ "ite" ++ show k) (map mkcon cs ++ [errcon, lprimcon, rprimcon, itecon, defcon])
--
----   errorN = Foo_Error
--errorD :: Name -> Int -> CG H.Dec
--errorD nm n = do
--  let body = H.NormalB $ H.VarE (qerrnmCG nm)
--      fun = H.ValD (H.VarP (H.mkName $ "error" ++ show n)) body []
--  return fun

--   realizeN = \m -> \x ->
--     case x of
--       (FooA x1 x2 ...) -> FooA (realize m x1) (realize m x2) ...
--       (FooB x1 x2 ...) -> FooB (realize m x1) (realize m x2) ...
--       ...
--       (Foo_Prim r _) -> r m
--       (Foo_Ite {}) -> flrealize m [__iteFooA x, __iteFooB x, ...]
--       (Foo_Error _) -> x
realizeD :: Name -> Int -> [DataCon] -> CG S.Method
realizeD n k cs = do
  addimport "Smten.Runtime.SmtenHS"
  let mkcon :: DataCon -> CG S.Alt
      mkcon d = do
        cn <- qnameCG $ dataConName d
        let nargs = length $ dataConOrigArgTys d
            xs = ["x" ++ show i | i <- [1..nargs]]
            pat = S.ConP cn xs
            rs = [foldl1 S.AppE [
                    S.VarE "Smten.Runtime.SmtenHS.realize",
                    S.VarE "m",
                    S.VarE x] | x <- xs]
            body = S.conE cn rs
        return $ S.Alt pat body

  qprimnm <- qprimnmCG n
  let primpat = S.ConP qprimnm ["r", "_"]
      primbody = S.AppE (S.VarE "r") (S.VarE "m")
      primcon = S.Alt primpat primbody

  qiteflnms <- mapM (qiteflnmCG . dataConName) cs
  qiteerrnm <- qiteerrnmCG n
  qitenm <- qitenmCG n
  let itecons = [S.AppE (S.VarE qiteflnm) (S.VarE "x") | qiteflnm <- qiteflnms]
      iteerr = S.AppE (S.VarE qiteerrnm) (S.VarE "x")
      itepat = S.RecP qitenm
      itebody = foldl1 S.AppE [
                    S.VarE $ "Smten.Runtime.SmtenHS.flrealize",
                    S.VarE $ "m",
                    S.ListE $ itecons ++ [iteerr]]
      itecon = S.Alt itepat itebody

  qerrnm <- qerrnmCG n
  let errpat = S.ConP qerrnm ["_"]
      errbody = S.VarE "x"
      errcon = S.Alt errpat errbody

  cons <- mapM mkcon cs
  let casee = S.CaseE (S.VarE "x") (cons ++ [primcon, itecon, errcon])
      body = S.LamE "m" $ (S.LamE "x") casee
  return $ S.Method ("realize" ++ show k) body
  
---- sappN f x@(Foo_Ite {}) = flsapp f x [__iteFooA x, __iteFooB x, ...]
---- sappN f (Foo_Error msg) = error0 msg
---- sappN f (Foo_Prim r c) = primsapp f r c
---- sappN f x = f x
--sappD :: Name -> Int -> [Con] -> CG H.Dec
--sappD n k cs = do
--  let primpats = [H.VarP $ H.mkName "f", H.ConP (qprimnmCG n) [H.VarP (H.mkName "r"), H.VarP (H.mkName "c")]]
--      primbody = foldl1 H.AppE [
--                    H.VarE $ H.mkName "Smten.primsapp",
--                    H.VarE $ H.mkName "f",
--                    H.VarE $ H.mkName "r",
--                    H.VarE $ H.mkName "c"]
--      primcon = H.Clause primpats (H.NormalB primbody) []
--
--      x = H.VarE $ H.mkName "x"
--
--      itecons = [H.AppE (H.VarE $ qiteflnmCG cn) x | Con cn _ <- cs]
--      iteerr = H.AppE (H.VarE $ qiteerrnmCG n) x
--      itepats = [H.VarP $ H.mkName "f", H.AsP (H.mkName "x") (H.RecP (qitenmCG n) [])]
--      itebody = foldl1 H.AppE [
--                    H.VarE $ H.mkName "Smten.flsapp",
--                    H.VarE $ H.mkName "f", x,
--                    H.ListE $ itecons ++ [iteerr]]
--      itecon = H.Clause itepats (H.NormalB itebody) []
--
--      errpats = [H.VarP $ H.mkName "f", H.ConP (qerrnmCG n) [H.VarP $ H.mkName "msg"]]
--      errbody = H.AppE (H.VarE $ H.mkName "Smten.error0") (H.VarE $ H.mkName "msg")
--      errcon = H.Clause errpats (H.NormalB errbody) []
--
--      defpats = [H.VarP $ H.mkName "f", H.VarP $ H.mkName "x"]
--      defbody = H.AppE (H.VarE $ H.mkName "f") (H.VarE $ H.mkName "x")
--      defcon = H.Clause defpats (H.NormalB defbody) []
--  return $ H.FunD (H.mkName $ "sapp" ++ show k) [itecon, errcon, primcon, defcon]
--
--mkIteHelpersD :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
--mkIteHelpersD n ts cs = do
--    null <- mkNullIteD n ts cs
--    to <- mkLiftIteD n ts cs
--    return $ concat [null, to]
--
---- __IteNullFoo :: Foo a b ...
---- __IteNullFoo = Foo_Ite {
----    __fl* = Nothing,
----    ...
---- }
--mkNullIteD :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
--mkNullIteD n ts cs = do
--  let dt = appsT (conT n) (map tyVarType ts)
--  H.SigD _ ty' <- topSigCG (TopSig (name "DONT_CARE") [] dt)
--  let sig = H.SigD (nullitenmCG n) ty'
--
--      fes = [(iteflnmCG cn, H.ConE $ H.mkName "Prelude.Nothing") | Con cn _ <- cs]
--      efe = (iteerrnmCG n, H.ConE $ H.mkName "Prelude.Nothing")
--      body = H.RecConE (qitenmCG n) (fes ++ [efe])
--      clause = H.Clause [] (H.NormalB body) []
--      fun = H.FunD (nullitenmCG n) [clause]
--  return [sig, fun]
--
---- __IteLiftFoo :: Foo a b ... -> Foo a b ...
---- __IteLiftFoo x@(FooA {}) = (__IteNullFoo :: Foo a b ...) {
----         __iteFooA = Just (True, x)
----         }
----  ...
---- __IteLiftFoo x@(Foo_Error msg) = (__IteNullFoo :: Foo a b ...) {
----         __iteErrFoo = Just (True, x)
----         }
---- __IteLiftFoo x@(Foo_Ite {}) = x
---- __IteLiftFoo (Foo_Prim {}) = Prelude.error "iteliftFoo.prim"
--mkLiftIteD :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
--mkLiftIteD n ts cs = do
--  let dt = appsT (conT n) (map tyVarType ts)
--      ty = arrowT dt dt
--  tyme <- typeCG dt
--  H.SigD _ ty' <- topSigCG (TopSig (name "DONT_CARE") [] ty)
--  let sig = H.SigD (liftitenmCG n) ty'
--
--      mkcon :: Con -> H.Clause
--      mkcon (Con cn cts) =
--        let pats = [H.AsP (H.mkName "x") (H.RecP (qnameCG cn) [])]
--            tuple = H.TupE [H.ConE $ H.mkName "Smten.True", H.VarE $ H.mkName "x"]
--            fields = [(iteflnmCG cn, H.AppE (H.ConE $ H.mkName "Prelude.Just") tuple)]
--            body = H.RecUpdE (H.SigE (H.VarE $ qnullitenmCG n) tyme) fields
--        in H.Clause pats (H.NormalB body) []
--
--      errpats = [H.AsP (H.mkName "x") (H.RecP (qerrnmCG n) [])]
--      errtuple = H.TupE [H.ConE $ H.mkName "Smten.True", H.VarE $ H.mkName "x"]
--      errfields = [(iteerrnmCG n, H.AppE (H.ConE $ H.mkName "Prelude.Just") errtuple)]
--      errbody = H.RecUpdE (H.SigE (H.VarE $ qnullitenmCG n) tyme) errfields
--      errcon = H.Clause errpats (H.NormalB errbody) []
--
--      itepats = [H.AsP (H.mkName "x") (H.RecP (qitenmCG n) [])]
--      itebody = H.VarE $ H.mkName "x"
--      itecon = H.Clause itepats (H.NormalB itebody) []
--
--      primpats = [H.RecP (qprimnmCG n) []]
--      primbody = H.AppE (H.VarE $ H.mkName "Prelude.error")
--                        (H.LitE $ H.StringL (H.nameBase (liftitenmCG n) ++ ".prim"))
--      primcon = H.Clause primpats (H.NormalB primbody) []
--
--      fun = H.FunD (liftitenmCG n) (map mkcon cs ++ [primcon, itecon, errcon])
--  return [sig, fun]
--
