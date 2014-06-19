
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
    cons <- mapM (mkConD n constrs) constrs
    return $ concat [dataD, cons, shsD]
  
-- data Foo a b ... = Foo {
--    gd_A :: BoolF, flA1 :: A1, flA2 :: A2, flA3 :: A3, ...
--    gd_B :: BoolF, flB1 :: B1, flB2 :: B2, flB3 :: B3, ...
--    ...
-- }
mkDataD :: Name -> [TyVar] -> [DataCon] -> CG [S.Dec]
mkDataD nm tyvars constrs = do
  boolnm <- usequalified "Smten.Runtime.Formula" "BoolF"
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
--   iteN = ...
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
   ite <- iteD nm n cs
   unreach <- unreachableD nm n cs
   traces <- traceD nm n cs
   return [S.InstD ctx ty [ite, unreach, traces]]

-- iteN = \p a b -> iteS p a b (Foo {
--   gd* = ite0 p (gd* a) (gd* b),
--   fl* = ite0 p (fl* a) (fl* b),
--   ...
-- })
iteD :: Name -> Int -> [DataCon] -> CG S.Method
iteD nm k cs = do
  itenm <- usequalified "Smten.Runtime.SmtenHS" "ite0"
  iteS <- S.VarE <$> usequalified "Smten.Runtime.Formula.BoolF" "iteS"
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
      ited = S.appsE iteS [S.VarE "p", S.VarE "a", S.VarE "b", upd]
      body = S.LamE "p" (S.LamE "a" (S.LamE "b" ited))
  return $ S.Method ("ite" ++ show k) body

-- unreachableN = Foo {
--   gd* = unreachable,
--   fl* = unreachable,
--   ...
-- }
unreachableD :: Name -> Int -> [DataCon] -> CG S.Method
unreachableD nm k cs = do
  xx <- S.VarE <$> usequalified "Smten.Runtime.SmtenHS" "unreachable"
  let mkcon :: DataCon -> CG [S.Name]
      mkcon d = do
        let dnm = dataConName d
            mkfield :: Int -> CG S.Name
            mkfield i = qfieldnmCG i dnm
        gdnm <- qguardnmCG dnm
        fields <- mapM (mkfield . fst) $ zip [1..] (dataConOrigArgTys d)
        return (gdnm : fields)

      mkupd :: S.Name -> S.Field
      mkupd nm = S.Field nm xx
  nm' <- qtynameCG nm
  ks <- concat <$> mapM mkcon cs
  let upds = map mkupd $ ks
      body = S.RecE (S.VarE nm') upds
  return $ S.Method ("unreachable" ++ show k) body

-- traceSN = \x ->
--    traceSD [("FooA", gdFooA x, [traceS0 (flFooA1 x), ...),
--             ("FooB", gdFooB x, [traceS0 (flFooB1 x), ...)]
traceD :: Name -> Int -> [DataCon] -> CG S.Method
traceD nm k cs = do
  trace <- S.VarE <$> usequalified "Smten.Runtime.SmtenHS" "traceS0"
  traced <- S.VarE <$> usequalified "Smten.Runtime.Trace" "traceSD"
  let mkcon :: DataCon -> CG S.Exp
      mkcon d = do
        let dnm = dataConName d

            mkfield :: Int -> CG S.Exp
            mkfield i = do
              flf <- S.VarE <$> qfieldnmCG i dnm
              return $ S.AppE trace (S.AppE flf (S.VarE "x"))

        let cstr = S.LitE (S.StringL (occNameString $ nameOccName dnm))

        gdf <- S.VarE <$> qguardnmCG dnm
        let gd = S.AppE gdf (S.VarE "x")

        fields <- S.ListE <$> (mapM (mkfield . fst) $ zip [1..] (dataConOrigArgTys d))
        return $ S.tup3E cstr gd fields

  ks <- S.ListE <$> mapM mkcon cs
  let body = S.LamE "x" (S.AppE traced ks)
  return $ S.Method ("traceS" ++ show k) body

-- mkConD n ds d 
-- Generate constructor functions for each constructor.
-- __FooA :: A1 -> A2 -> ... Foo a b ...
-- __FooA = \a b ... -> Foo {
--      gdA = trueF, flA1 = a, flA2 = b, ...,
--      gdB = falseF, flB1 = unreachable, flB2 = unreachabe, ...,
--      ...
--   }
--  n - the name of the type constructor: eg "Foo"
--  ds - the list of all data constructors
--  d - the specific data constructor to generate the constructor function for
mkConD :: Name -> [DataCon] -> DataCon -> CG S.Dec
mkConD tynm cs dc = do
  tt <- S.VarE <$> usequalified "Smten.Runtime.Formula" "trueF"
  ff <- S.VarE <$> usequalified "Smten.Runtime.Formula" "falseF"
  xx <- S.VarE <$> usequalified "Smten.Runtime.SmtenHS" "unreachable"
  tynm' <- S.VarE <$> qtynameCG tynm
  ty <- typeCG $ dataConUserType dc
  let mkcon :: DataCon -> CG [S.Field]
      mkcon d
        | d == dc = do
            let dnm = dataConName d
            gdnm <- guardnmCG dnm
            let numargs = length $ dataConOrigArgTys d
                vars = ["x" ++ show i | i <- [1..numargs]]
            fields <- mapM (flip fieldnmCG dnm) [1..numargs]
            let flupds = [S.Field fl (S.VarE v) | (fl, v) <- zip fields vars]
            return (S.Field gdnm tt : flupds)
        | otherwise = do
            let dnm = dataConName d
                mkfield :: Int -> CG S.Field
                mkfield i = do
                  fnm <- qfieldnmCG i dnm
                  return $ S.Field fnm xx
            gdnm <- qguardnmCG dnm
            fields <- mapM (mkfield . fst) $ zip [1..] (dataConOrigArgTys d)
            return (S.Field gdnm ff : fields)

  upds <- concat <$> mapM mkcon cs
  nm <- connmCG $ dataConName dc
  let numargs = length $ dataConOrigArgTys dc
      vars = ["x" ++ show i | i <- [1..numargs]]
      upd = S.RecE tynm' upds
      body = foldr S.LamE upd vars
  return $ S.ValD (S.Val nm (Just ty) body)

