
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
        
 | Just cs <- tyConDataCons_maybe t = dataCG t cs
 | isSynTyCon t = return []
 | otherwise = do
      lift $ errorMsg (text "Unsupported TyCon in tyconCG: " <+> ppr t)
      return []

dataCG :: TyCon -> [DataCon] -> CG [S.Dec]
dataCG t constrs = do
    let n = tyConName t
        tyvars = tyConTyVars t
    dataD <- mkDataD n tyvars constrs
    shsD <- smtenHS n tyvars constrs
    itehelpers <- mkIteHelpersD t n tyvars constrs
    return $ concat [dataD, itehelpers, shsD]
  
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
   ite <- iteD nm n cs
   err <- errorD nm n
   --sapp <- sappD nm n cs
   --return [S.InstD ctx ty [rel, ite, prim, err, sapp]]
   return [S.InstD ctx ty [rel, ite, err]]

--   primN = Foo_Prim
primD :: Name -> Int -> CG S.Method
primD nm n = do
  qprimnm <- qprimnmCG nm
  return $ S.Method ("primitive" ++ show n) (S.VarE qprimnm)

-- iteN = \p a b ->
--   case (a, b) of
--      (FooA a1 a2 ..., FooA b1 b2 ...) -> FooA (ite p a1 b1) (ite p a2 b2) ...
--      (FooB a1 a2 ..., FooB b1 b2 ...) -> FooB (ite p a1 b1) (ite p a2 b2) ...
--      ...
--      (Foo_Error a1, Foo_Error b1) -> Foo_Error (ite p a1 b1)
--      (Foo_Ite {}, Foo_Ite {}) -> Foo_Ite {
--           __iteFooA = flmerge p (__iteFooA a) (__iteFooA b)
--           __iteFooB = flmerge p (__iteFooB a) (__iteFooB b)
--           ...
--         }
--      (Foo_Prim r c, _) -> Foo_Prim (iterealize p a b) (ite p c b)
--      (_, Foo_Prim r c) -> Foo_Prim (iterealize p a b) (ite p a c)
--      _ -> ite p (__LiftIteFoo a) (__LiftIteFoo b)
iteD :: Name -> Int -> [DataCon] -> CG S.Method
iteD n k cs = do
  addimport "Smten.Runtime.SmtenHS"
  let ite a b = foldl1 S.AppE [S.VarE "Smten.Runtime.SmtenHS.ite", S.VarE "p", a, b]

      mkcon :: DataCon -> CG S.Alt
      mkcon d = do
        cn <- qnameCG $ dataConName d
        let nargs = length (dataConOrigArgTys d)
            as = ["a" ++ show i | i <- [1..nargs]]
            bs = ["b" ++ show i | i <- [1..nargs]]
            pat = S.tup2P (S.ConP cn (map S.VarP as)) (S.ConP cn (map S.VarP bs))
            ites = [ite (S.VarE a) (S.VarE b) | (a, b) <- zip as bs]
            body = S.conE cn ites
        return $ S.Alt pat body

  qerrnm <- qerrnmCG n
  let errpat = S.tup2P (S.ConP qerrnm [S.VarP "a"]) (S.ConP qerrnm [S.VarP "b"])
      errbody = S.AppE (S.VarE qerrnm) (ite (S.VarE "a") (S.VarE "b"))
      errcon = S.Alt errpat errbody

      mkfe :: DataCon -> CG S.Field
      mkfe d = do
        iteflnm <- iteflnmCG $ dataConName d
        qiteflnm <- qiteflnmCG $ dataConName d
        return $ S.Field iteflnm (foldl1 S.AppE [
                        S.VarE "Smten.Runtime.SmtenHS.flmerge",
                        S.VarE "p",
                        S.AppE (S.VarE qiteflnm) (S.VarE "a"),
                        S.AppE (S.VarE qiteflnm) (S.VarE "b")])

  iteerrnm <- iteerrnmCG n
  qiteerrnm <- qiteerrnmCG n
  let efe = S.Field iteerrnm (foldl1 S.AppE [
                        S.VarE "Smten.Runtime.SmtenHS.flmerge",
                        S.VarE "p",
                        S.AppE (S.VarE qiteerrnm) (S.VarE "a"),
                        S.AppE (S.VarE qiteerrnm) (S.VarE "b")])
  qitenm <- qitenmCG n
  fes <- mapM mkfe cs
  let itepat = S.tup2P (S.RecP qitenm) (S.RecP qitenm)
      itebody = S.RecE (S.VarE qitenm) (fes ++ [efe])
      itecon = S.Alt itepat itebody

  qprimnm <- qprimnmCG n
  let lprimpat = S.tup2P (S.ConP qprimnm [S.VarP "r", S.VarP "c"]) S.wildP
      lprimbody = S.conE qprimnm [
         foldl1 S.AppE (map S.VarE ["Smten.Runtime.SmtenHS.iterealize", "p", "a", "b"]),
         ite (S.VarE "c") (S.VarE "b")]
      lprimcon = S.Alt lprimpat lprimbody

      rprimpat = S.tup2P S.wildP (S.ConP qprimnm [S.VarP "r", S.VarP "c"])
      rprimbody = S.conE qprimnm [
         foldl1 S.AppE (map S.VarE ["Smten.Runtime.SmtenHS.iterealize", "p", "a", "b"]),
         ite (S.VarE "a") (S.VarE "c")]
      rprimcon = S.Alt rprimpat rprimbody

  qliftitenm <- qliftitenmCG n
  let defpat = S.wildP
      defbody = ite (S.AppE (S.VarE qliftitenm) (S.VarE "a"))
                    (S.AppE (S.VarE qliftitenm) (S.VarE "b"))
      defcon = S.Alt defpat defbody

  cons <- mapM mkcon cs
  let casee = S.CaseE (S.tup2E (S.VarE "a") (S.VarE "b")) (cons ++ [errcon, lprimcon, rprimcon, itecon, defcon])
      body = S.LamE "p" (S.LamE "a" (S.LamE "b" casee))
  return $ S.Method ("ite" ++ show k) body

--   errorN = Foo_Error
errorD :: Name -> Int -> CG S.Method
errorD nm n = do
  qerrnm <- qerrnmCG nm
  return $ S.Method ("error" ++ show n) (S.VarE qerrnm)

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
            pat = S.ConP cn (map S.VarP xs)
            rs = [foldl1 S.AppE [
                    S.VarE "Smten.Runtime.SmtenHS.realize",
                    S.VarE "m",
                    S.VarE x] | x <- xs]
            body = S.conE cn rs
        return $ S.Alt pat body

  qprimnm <- qprimnmCG n
  let primpat = S.ConP qprimnm [S.VarP "r", S.wildP]
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
  let errpat = S.ConP qerrnm [S.wildP]
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

mkIteHelpersD :: TyCon -> Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkIteHelpersD t n ts cs = do
    null <- mkNullIteD t n ts cs
    to <- mkLiftIteD t n ts cs
    return $ concat [null, to]

-- __IteNullFoo :: Foo a b ...
-- __IteNullFoo = Foo_Ite {
--    __fl* = Nothing,
--    ...
-- }
mkNullIteD :: TyCon -> Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkNullIteD t n ts cs = do
  let dt = mkTyConApp t (mkTyVarTys ts)
  ty <- topTypeCG dt
  nullitenm <- nullitenmCG n
  iteflnms <- mapM (iteflnmCG . dataConName) cs
  iteerrnm <- iteerrnmCG n
  qitenm <- qitenmCG n
  let fes = [S.Field iteflnm (S.conE "Prelude.Nothing" []) | iteflnm <- iteflnms]
      efe = S.Field iteerrnm (S.conE "Prelude.Nothing" [])
      body = S.RecE (S.VarE qitenm) (fes ++ [efe])
  return [S.ValD (S.Val nullitenm ty body)]

-- __IteLiftFoo :: Foo a b ... -> Foo a b ...
-- __IteLiftFoo = \x ->
--    case x of
--      FooA {} -> (__IteNullFoo :: Foo a b ...) {
--                      __iteFooA = Just (True, x)
--                   }
--      ...
--      Foo_Error msg -> (__IteNullFoo :: Foo a b ...) {
--                      __iteErrFoo = Just (True, x)
--                   }
--      Foo_Ite {} -> x
mkLiftIteD :: TyCon -> Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkLiftIteD t n ts cs = do
  let dt = mkTyConApp t (mkTyVarTys ts)
      ty = mkFunTy dt dt
  tyme <- typeCG dt
  ty' <- topTypeCG ty
  liftitenm <- liftitenmCG n
  qnullitenm <- qnullitenmCG n
  let mkcon :: DataCon -> CG S.Alt
      mkcon d = do
        let cn = dataConName d
        qname <- qnameCG cn
        iteflnm <- iteflnmCG cn
        let pat = S.RecP qname
            tuple = S.tup2E (S.conE "Smten.Runtime.Formula.TrueF" []) (S.VarE "x")
            fields = [S.Field iteflnm (S.conE "Prelude.Just" [tuple])]
            body = S.RecE (S.SigE (S.VarE qnullitenm) tyme) fields
        return $ S.Alt pat body

  qerrnm <- qerrnmCG n
  iteerrnm <- iteerrnmCG n
  let errpat = S.RecP qerrnm
      errtuple = S.tup2E (S.conE "Smten.Runtime.Formula.TrueF" []) (S.VarE "x")
      errfields = [S.Field iteerrnm (S.conE "Prelude.Just" [errtuple])]
      errbody = S.RecE (S.SigE (S.VarE qnullitenm) tyme) errfields
      errcon = S.Alt errpat errbody

  qitenm <- qitenmCG n
  let itepat = S.RecP qitenm
      itebody = S.VarE "x"
      itecon = S.Alt itepat itebody

  cons <- mapM mkcon cs
  let casee = S.CaseE (S.VarE "x") (cons ++ [itecon, errcon])
      body = S.LamE "x" casee
  return [S.ValD (S.Val liftitenm ty' body)]

