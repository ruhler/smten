
module Smten.Plugin.Data (
    dataCG,
  ) where

import GhcPlugins

import Control.Monad
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
    nulls <- mkNullD t n tyvars constrs
    cons <- mapM (mkConD n) constrs
    return $ concat [dataD, nulls, cons, shsD]
  
-- data Foo a b ... = Foo {
--    gd_A :: Bool, flA1 :: A1, flA2 :: A2, flA3 :: A3, ...
--    gd_B :: Bool, flB1 :: B1, flB2 :: B2, flB3 :: B3, ...
--    ...
-- }
mkDataD :: Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkDataD nm tyvars constrs = do
  boolnm <- usequalified "Smten.Runtime.Formula" "Bool"
  let boolty = S.ConAppT boolnm []
      mkcon :: DataCon -> CG [S.RecField]
      mkcon d = do
        let dnm = dataConName d

            mkfield :: (Int, Type) -> CG S.RecField
            mkfield (i, t) = liftM2 S.RecField (fieldnmCG i dnm) (typeCG t)
        gdnm <- guardnmCG dnm
        fields <- mapM mkfield $ zip [1..] (dataConOrigArgTys d)
        return (S.RecField gdnm boolty : fields)

  nm' <- tynameCG nm
  vs <- mapM (qnameCG . varName) tyvars
  fields <- concat <$> mapM mkcon constrs
  let con = S.RecC nm' fields
  return [S.DataD (S.Data nm' vs [con])]

-- Note: we currently don't support crazy kinded instances of SmtenHS. This
-- means we are limited to "linear" kinds of the form (* -> * -> ... -> *)
--
-- To handle that properly, we chop off as many type variables as needed to
-- get to a linear kind.
--   call the chopped off type variables c1, c2, ...
--
-- instance (SmtenN c1, SmtenN c2, ...) => SmtenHSN (Foo c1 c2 ...) where
--   realizeN = ...
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
   return [S.InstD ctx ty [rel, ite]]

-- iteN = \p a b -> Foo {
--   gd* = ite0 p (gd* a) (gd* b),
--   fl* = ite0 p (fl* a) (fl* b),
--   ...
-- }
iteD :: Name -> Int -> [DataCon] -> CG S.Method
iteD nm k cs = do
  itenm <- usequalified "Smten.Runtime.SmtenHS" "ite0"
  nm' <- qtynameCG nm
  let mkcon :: DataCon -> CG [S.Name]
      mkcon d = do
        let dnm = dataConName d
            mkfield :: Int -> CG S.Name
            mkfield i = qfieldnmCG i dnm
        gdnm <- qguardnmCG dnm
        fields <- mapM (mkfield . fst) $ zip [1..] (dataConOrigArgTys d)
        return (gdnm : fields)

      mkupd :: S.Name -> S.Field
      mkupd nm = S.Field nm $ foldl1 S.AppE [
                        S.VarE itenm,
                        S.VarE "p",
                        S.AppE (S.VarE nm) (S.VarE "a"),
                        S.AppE (S.VarE nm) (S.VarE "b")]

  nm' <- qtynameCG nm
  ks <- concat <$> mapM mkcon cs
  let upds = map mkupd $ ks
      upd = S.RecE (S.VarE nm') upds
      body = S.LamE "p" (S.LamE "a" (S.LamE "b" upd))
  return $ S.Method ("ite" ++ show k) body

--   realizeN = \m -> \x -> Foo {
--     gd* = realize m (gd* x),
--     fl* = realize m (fl* x),
--     ...
--   }
realizeD :: Name -> Int -> [DataCon] -> CG S.Method
realizeD nm k cs = do
  relnm <- usequalified "Smten.Runtime.SmtenHS" "realize"
  nm' <- qtynameCG nm
  let mkcon :: DataCon -> CG [S.Name]
      mkcon d = do
        let dnm = dataConName d
            mkfield :: Int -> CG S.Name
            mkfield i = qfieldnmCG i dnm
        gdnm <- qguardnmCG dnm
        fields <- mapM (mkfield . fst) $ zip [1..] (dataConOrigArgTys d)
        return (gdnm : fields)

      mkupd :: S.Name -> S.Field
      mkupd nm = S.Field nm $ foldl1 S.AppE [
                        S.VarE relnm,
                        S.VarE "m",
                        S.AppE (S.VarE nm) (S.VarE "x")]

  nm' <- qtynameCG nm
  ks <- concat <$> mapM mkcon cs
  let upds = map mkupd $ ks
      upd = S.RecE (S.VarE nm') upds
      body = S.LamE "m" (S.LamE "x" upd)
  return $ S.Method ("realize" ++ show k) body

-- __NullFoo :: Foo a b ...
-- __NullFoo = Foo {
--    gd* = False,
--    fl* = unusedfield "*"
-- }
mkNullD :: TyCon -> Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkNullD t nm ts cs = do
  unused <- usequalified "Smten.Runtime.SmtenHS" "unusedfield"
  false <- usequalified "Smten.Runtime.Formula" "False"
  nm' <- qtynameCG nm
  let mkcon :: DataCon -> CG [S.Field]
      mkcon d = do
        let dnm = dataConName d
            mkfield :: Int -> CG S.Field
            mkfield i = do
              fnm <- qfieldnmCG i dnm
              return $ S.Field fnm (S.AppE (S.VarE unused) (S.LitE (S.StringL fnm)))
        gdnm <- qguardnmCG dnm
        fields <- mapM (mkfield . fst) $ zip [1..] (dataConOrigArgTys d)
        return (S.Field gdnm (S.VarE false) : fields)

  upds <- concat <$> mapM mkcon cs
  let body = S.RecE (S.VarE nm') upds
      dt = mkTyConApp t (mkTyVarTys ts)
  ty <- topTypeCG dt
  nullnm <- nullnmCG nm
  return [S.ValD (S.Val nullnm (Just ty) body)]

-- Generate constructor functions for each constructor.
-- __FooA :: A1 -> A2 -> ... Foo a b ...
-- __FooA = \a b ... -> __NullFoo { gdA = True, flA1 = a, flA2 = b, ... }
mkConD :: Name -> DataCon -> CG S.Dec
mkConD tynm d = do
  let dnm = dataConName d
  nm <- connmCG dnm
  ty <- typeCG $ dataConUserType d
  null <- S.VarE <$> nullnmCG tynm
  gdnm <- guardnmCG dnm
  tt <- S.VarE <$> usequalified "Smten.Runtime.Formula" "True"
  let gdupd = S.Field gdnm tt
      numargs = length $ dataConOrigArgTys d
      vars = ["x" ++ show i | i <- [1..numargs]]
  fields <- mapM (flip fieldnmCG dnm) [1..numargs]
  let flupds = [S.Field fl (S.VarE v) | (fl, v) <- zip fields vars]
      upd = S.RecE null (gdupd : flupds)
      body = foldr S.LamE upd vars
  return $ S.ValD (S.Val nm (Just ty) body)

