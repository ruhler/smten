
module Smten.Plugin.Data (
    dataCG,
  ) where

import GhcPlugins

import Data.Functor

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type
import qualified Smten.Plugin.Output.Syntax as S

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
--                  | Foo_Error ErrorString
--                  | Foo_Ite {
--                      __iteFooA :: (BoolF, Foo a b ...),
--                      __iteFooB :: (BoolF, Foo a b ...),
--                      ...,
--                      __iteFoo_Error :: (BoolF, Foo a b ...)
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
  errnm <- errnmCG nm
  iteerrnm <- iteerrnmCG nm
  iteflnms <- mapM (iteflnmCG . dataConName) constrs
  itenm <- itenmCG nm
  errstrnm <- usequalified "Smten.Runtime.Types" "ErrorString"
  boolnm <- usequalified "Smten.Runtime.Types" "Bool"
  let tyme = S.ConAppT qtyname tyvs'
      err = S.Con errnm [S.ConAppT errstrnm []]

      tybool = S.ConAppT boolnm []
      tyfield = S.ConAppT "(,)" [tybool, tyme]

      iteerr = S.RecField iteerrnm tyfield
      ites = [S.RecField iteflnm tyfield | iteflnm <- iteflnms]
      ite = S.RecC itenm (ites ++ [iteerr])

      allks = ks ++ [err, ite]
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
--   errorN = ...
--   ...
smtenHS :: Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
smtenHS nm tyvs cs = do
   let (rkept, rdropped) = span (isStarKind . tyVarKind) (reverse tyvs)
       n = length rkept
       dropped = reverse rdropped
   tyvs' <- mapM tyvarCG dropped
   qtyname <- qtynameCG nm
   ctx <- concat <$> mapM ctxCG dropped
   smtenhsnm <- usequalified "Smten.Runtime.SmtenHS" ("SmtenHS" ++ show n)
   let ty = S.ConAppT smtenhsnm [S.ConAppT qtyname tyvs']
   rel <- realizeD nm n cs
   ite <- iteD nm n cs
   err <- errorD nm n
   return [S.InstD ctx ty [rel, ite, err]]

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
--      _ -> ite p (__LiftIteFoo a) (__LiftIteFoo b)
iteD :: Name -> Int -> [DataCon] -> CG S.Method
iteD n k cs = do
  itenm <- usequalified "Smten.Runtime.SmtenHS" "ite"
  flmergenm <- usequalified "Smten.Runtime.SmtenHS" "flmerge"
  let ite a b = foldl1 S.AppE [S.VarE itenm, S.VarE "p", a, b]

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
                        S.VarE flmergenm,
                        S.VarE "p",
                        S.AppE (S.VarE qiteflnm) (S.VarE "a"),
                        S.AppE (S.VarE qiteflnm) (S.VarE "b")])

  iteerrnm <- iteerrnmCG n
  qiteerrnm <- qiteerrnmCG n
  let efe = S.Field iteerrnm (foldl1 S.AppE [
                        S.VarE flmergenm,
                        S.VarE "p",
                        S.AppE (S.VarE qiteerrnm) (S.VarE "a"),
                        S.AppE (S.VarE qiteerrnm) (S.VarE "b")])
  qitenm <- qitenmCG n
  fes <- mapM mkfe cs
  let itepat = S.tup2P (S.RecP qitenm) (S.RecP qitenm)
      itebody = S.RecE (S.VarE qitenm) (fes ++ [efe])
      itecon = S.Alt itepat itebody

  qliftitenm <- qliftitenmCG n
  let defpat = S.wildP
      defbody = ite (S.AppE (S.VarE qliftitenm) (S.VarE "a"))
                    (S.AppE (S.VarE qliftitenm) (S.VarE "b"))
      defcon = S.Alt defpat defbody

  cons <- mapM mkcon cs
  let casee = S.CaseE (S.tup2E (S.VarE "a") (S.VarE "b")) (cons ++ [errcon, itecon, defcon])
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
--       (Foo_Ite {}) -> flrealize m [__iteFooA x, __iteFooB x, ...]
--       (Foo_Error _) -> x
realizeD :: Name -> Int -> [DataCon] -> CG S.Method
realizeD n k cs = do
  realizenm <- usequalified "Smten.Runtime.SmtenHS" "realize"
  flrealizenm <- usequalified "Smten.Runtime.SmtenHS" "flrealize"
  let mkcon :: DataCon -> CG S.Alt
      mkcon d = do
        cn <- qnameCG $ dataConName d
        let nargs = length $ dataConOrigArgTys d
            xs = ["x" ++ show i | i <- [1..nargs]]
            pat = S.ConP cn (map S.VarP xs)
            rs = [foldl1 S.AppE [
                    S.VarE realizenm,
                    S.VarE "m",
                    S.VarE x] | x <- xs]
            body = S.conE cn rs
        return $ S.Alt pat body

  qiteflnms <- mapM (qiteflnmCG . dataConName) cs
  qiteerrnm <- qiteerrnmCG n
  qitenm <- qitenmCG n
  let itecons = [S.AppE (S.VarE qiteflnm) (S.VarE "x") | qiteflnm <- qiteflnms]
      iteerr = S.AppE (S.VarE qiteerrnm) (S.VarE "x")
      itepat = S.RecP qitenm
      itebody = foldl1 S.AppE [
                    S.VarE $ flrealizenm,
                    S.VarE $ "m",
                    S.ListE $ itecons ++ [iteerr]]
      itecon = S.Alt itepat itebody

  qerrnm <- qerrnmCG n
  let errpat = S.ConP qerrnm [S.wildP]
      errbody = S.VarE "x"
      errcon = S.Alt errpat errbody

  cons <- mapM mkcon cs
  let casee = S.CaseE (S.VarE "x") (cons ++ [itecon, errcon])
      body = S.LamE "m" $ (S.LamE "x") casee
  return $ S.Method ("realize" ++ show k) body
  
mkIteHelpersD :: TyCon -> Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkIteHelpersD t n ts cs = do
    null <- mkNullIteD t n ts cs
    to <- mkLiftIteD t n ts cs
    return $ concat [null, to]

-- __IteNullFoo :: Foo a b ...
-- __IteNullFoo = Foo_Ite {
--    __fl* = unusedfield,
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
  unusednm <- usequalified "Smten.Runtime.SmtenHS" "unusedfield"
  let nothing = S.VarE unusednm
      fes = [S.Field iteflnm nothing | iteflnm <- iteflnms]
      efe = S.Field iteerrnm nothing
      body = S.RecE (S.VarE qitenm) (fes ++ [efe])
  return [S.ValD (S.Val nullitenm (Just ty) body)]

-- __IteLiftFoo :: Foo a b ... -> Foo a b ...
-- __IteLiftFoo = \x ->
--    case x of
--      FooA {} -> (__IteNullFoo :: Foo a b ...) {
--                      __iteFooA = (True, x)
--                   }
--      ...
--      Foo_Error msg -> (__IteNullFoo :: Foo a b ...) {
--                      __iteErrFoo = (True, x)
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
            tuple = S.tup2E (S.conE "Smten.Runtime.Types.True" []) (S.VarE "x")
            fields = [S.Field iteflnm tuple]
            body = S.RecE (S.SigE (S.VarE qnullitenm) tyme) fields
        return $ S.Alt pat body

  qerrnm <- qerrnmCG n
  iteerrnm <- iteerrnmCG n
  let errpat = S.RecP qerrnm
      errtuple = S.tup2E (S.conE "Smten.Runtime.Types.True" []) (S.VarE "x")
      errfields = [S.Field iteerrnm errtuple]
      errbody = S.RecE (S.SigE (S.VarE qnullitenm) tyme) errfields
      errcon = S.Alt errpat errbody

  qitenm <- qitenmCG n
  let itepat = S.RecP qitenm
      itebody = S.VarE "x"
      itecon = S.Alt itepat itebody

  cons <- mapM mkcon cs
  let casee = S.CaseE (S.VarE "x") (cons ++ [itecon, errcon])
      body = S.LamE "x" casee
  return [S.ValD (S.Val liftitenm (Just ty') body)]

