
module Smten.CodeGen.Data (
    dataCG,
    ) where

import qualified Language.Haskell.TH.Syntax as H

import Data.Functor((<$>))

import Smten.Name
import Smten.Type
import Smten.Dec
import Smten.CodeGen.CG
import Smten.CodeGen.Type
import Smten.CodeGen.Name

haskellys :: [Name]
haskellys = [unitN, listN, maybeN, name "Smten.Symbolic.Solver"]

dataCG :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
dataCG n tyvars constrs = do
    dataD <- mkDataD n tyvars constrs
    casesD <- concat <$> mapM (mkCaseD n tyvars) constrs
    shsD <- smtenHS n tyvars constrs
    haskellyD <-
      if n `elem` haskellys 
        then mkHaskellyD n tyvars constrs
        else return []
    return $ concat [dataD, casesD, shsD, haskellyD]

-- data Foo a b ... = FooA A1 A2 ...
--                  | FooB B1 B2 ...
--                  ...
--                  | FooK K1 K2 ...
--                  | FooMux__ Bool (Foo a b ...) (Foo a b ...)
mkDataD :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
mkDataD n tyvars constrs = do
  let tyvars' = [H.PlainTV (nameCG nm) | TyVar nm _ <- tyvars]

      mkcon :: Con -> CG H.Con
      mkcon (Con cn tys) = do
        let cn' = nameCG cn
        tys' <- mapM typeCG tys
        return (H.NormalC cn' [(H.NotStrict, ty') | ty' <- tys'])

      tyme = foldl H.AppT (H.ConT (qtynameCG n)) [H.VarT (nameCG nm) | TyVar nm _ <- tyvars]
      tys = [H.ConT (qtynameCG boolN), tyme, tyme]
      mux = H.NormalC (muxnmCG n) [(H.NotStrict, ty) | ty <- tys]
  constrs' <- mapM mkcon constrs
  return [H.DataD [] (tynameCG n) tyvars' (constrs' ++ [mux]) []]

-- __caseFooX :: Foo a b ... -> (X1 -> X2 -> ... -> z__) -> z__ -> z__
-- __caseFooX x y n =
--    case x of
--      FooX x1 x2 ... -> y x1 x2 ...
--      FooMux__ p a b -> strict_app0 (\v -> __caseFooX v y n) x
--      _ -> n
mkCaseD :: Name -> [TyVar] -> Con -> CG [H.Dec]
mkCaseD n tyvars (Con cn tys) = do
  let dt = appsT (conT n) (map tyVarType tyvars)
      zt = varT (name "z__")
      yt = arrowsT (tys ++ [zt])
      ty = arrowsT [dt, yt, zt, zt]
  H.SigD _ ty' <- topSigCG (TopSig (name "DONT_CARE") [] ty)
  let sig = H.SigD (casenmCG cn) ty'

      [vx, vy, vn] = map H.mkName ["x", "y", "n"]
      vxs = [H.mkName ("x" ++ show i) | i <- [1..(length tys)]]
      matchy = H.Match (H.ConP (qnameCG cn) (map H.VarP vxs))
                       (H.NormalB (foldl H.AppE (H.VarE vy) (map H.VarE vxs))) []

      mbody = foldl1 H.AppE [
         H.VarE (H.mkName "Smten.strict_app0"),
         H.LamE [H.VarP $ H.mkName "v"] $
             foldl H.AppE (H.VarE (qcasenmCG cn)) [H.VarE $ H.mkName v | v <- ["v", "y", "n"]],
         H.VarE $ H.mkName "x"]
      matchm = H.Match (H.ConP (qmuxnmCG n) [H.VarP $ H.mkName v | v <- ["p", "a", "b"]]) (H.NormalB mbody) []

      matchn = H.Match H.WildP (H.NormalB (H.VarE vn)) []
      cse = H.CaseE (H.VarE vx) [matchy, matchm, matchn]

      clause = H.Clause (map H.VarP [vx, vy, vn]) (H.NormalB cse) []
      fun = H.FunD (casenmCG cn) [clause]
  return [sig, fun]

-- instance (Haskelly ha sa, Haskelly hb sb, ...) =>
--   Haskelly (Foo ha hb ...) (Smten.Lib.Foo sa sb ...) where
--     frhs ...
--     tohs ...
mkHaskellyD :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
mkHaskellyD nm tyvars cons = do
  let hvars = [H.VarT (H.mkName $ "h" ++ unname n) | TyVar n _ <- tyvars]
      svars = [H.VarT (H.mkName $ "s" ++ unname n) | TyVar n _ <- tyvars]
      ctx = [H.ClassP (H.mkName "Smten.Haskelly") [ht, st] | (ht, st) <- zip hvars svars]
      ht = foldl H.AppT (H.ConT (qhstynameCG nm)) hvars
      st = foldl H.AppT (H.ConT (qtynameCG nm)) svars
      ty = foldl1 H.AppT [H.ConT $ H.mkName "Smten.Haskelly", ht, st]
  frhs <- mkFrhsD cons
  tohs <- mkTohsD cons
  return [H.InstanceD ctx ty [frhs, tohs]]

--     frhs (FooA x1 x2 ...) = Smten.Lib.FooA (frhs x1) (frhs x2) ...
--     frhs (FooB x1 x2 ...) = Smten.Lib.FooB (frhs xs) (frhs x2) ...
--     ...
mkFrhsD :: [Con] -> CG H.Dec
mkFrhsD cons = do
  let mkcon :: Con -> H.Clause
      mkcon (Con cn tys) = 
        let xs = [H.mkName $ "x" ++ show i | i <- [1..(length tys)]]
            pat = H.ConP (qhsnameCG cn) (map H.VarP xs)
            body = foldl H.AppE (H.ConE (qnameCG cn)) [H.AppE (H.VarE $ H.mkName "Smten.frhs") (H.VarE x) | x <- xs]
        in H.Clause [pat] (H.NormalB body) []
  return $ H.FunD (H.mkName "frhs") (map mkcon cons)

--     tohs (Smten.Lib.FooA x1 x2 ...) = do
--        x1' <- tohs x1
--        x2' <- tohs x2
--        ...
--        return (FooA x1' x2' ...)
--     tohs (Smten.Lib.FooB x1 x2 ...) = do
--        x1' <- tohs x1
--        x2' <- tohs x2
--        ...
--        return (FooB x1' x2' ...)
--     ...
--     tohs _ = Nothing
mkTohsD :: [Con] -> CG H.Dec
mkTohsD cons = do
  let mkcon :: Con -> H.Clause
      mkcon (Con cn tys) = 
        let xs = [H.mkName $ "x" ++ show i | i <- [1..(length tys)]]
            xs' = [H.mkName $ "x" ++ show i ++ "'" | i <- [1..(length tys)]]
            pat = H.ConP (qnameCG cn) (map H.VarP xs)
            stmts = [H.BindS (H.VarP x') (H.AppE (H.VarE (H.mkName "Smten.tohs")) (H.VarE x)) | (x, x') <- zip xs xs']
            rtn = H.NoBindS $ H.AppE (H.VarE (H.mkName "Prelude.return"))
                                     (foldl H.AppE (H.ConE (qhsnameCG cn)) (map H.VarE xs'))
            body = H.DoE (stmts ++ [rtn])
        in H.Clause [pat] (H.NormalB body) []
      def = H.Clause [H.WildP] (H.NormalB (H.VarE (H.mkName "Prelude.Nothing"))) []
  return $ H.FunD (H.mkName "tohs") (map mkcon cons ++ [def])

-- instance SmtenHSN Foo where
--   muxN = ...
--   realizeN = ...
--   strict_appN = ...
smtenHS :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
smtenHS nm tyvs cs = do
   let n = length tyvs
       ty = H.AppT (H.VarT (H.mkName $ "Smten.SmtenHS" ++ show n))
                   (H.ConT $ qtynameCG nm)
   mux <- muxD nm tyvs
   rel <- realizeD nm tyvs cs
   app <- appD nm tyvs cs
   return [H.InstanceD [] ty [mux, rel, app]]

--   muxN = FooMux__
muxD :: Name -> [TyVar] -> CG H.Dec
muxD nm tys = do
  let n = length tys
      body = H.NormalB $ H.VarE (qmuxnmCG nm)
      fun = H.ValD (H.VarP (H.mkName $ "mux" ++ show n)) body []
  return fun

--   realizeN m (FooA x1 x2 ...) = FooA (realize0 m x1) (realize0 m x2) ...
--   realizeN m (FooB x1 x2 ...) = FooB (realize0 m x1) (realize0 m x2) ...
--   ...
--   realizeN m (FooMux__ p a b) = __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
realizeD :: Name -> [TyVar] -> [Con] -> CG H.Dec
realizeD n tys cs = do
  let mkcon :: Con -> H.Clause
      mkcon (Con cn cts) =
        let xs = [H.mkName $ "x" ++ show i | i <- [1..length cts]]
            pats = [H.VarP $ H.mkName "m", H.ConP (qnameCG cn) (map H.VarP xs)]
            rs = [foldl1 H.AppE [
                    H.VarE (H.mkName "Smten.realize0"),
                    H.VarE (H.mkName "m"),
                    H.VarE x] | x <- xs]
            body = foldl H.AppE (H.ConE (qnameCG cn)) rs
        in H.Clause pats (H.NormalB body) []

      mxpats = [H.VarP $ H.mkName "m", H.ConP (qmuxnmCG n) [H.VarP (H.mkName v) | v <- ["p", "a", "b"]]]
      mxrs = [foldl1 H.AppE [
                H.VarE (H.mkName "Smten.realize0"),
                H.VarE (H.mkName "m"),
                H.VarE (H.mkName v)] | v <- ["p", "a", "b"]]
      mxbody = foldl H.AppE (H.VarE (qcasenmCG trueN)) mxrs
      mxcon = H.Clause mxpats (H.NormalB mxbody) []
  return $ H.FunD (H.mkName $ "realize" ++ show (length tys)) (map mkcon cs ++ [mxcon])

-- strict_appN f (FooMux__ p a b) = mux0 p (strict_app0 f a) (strict_app0 f b) 
-- strict_appN f x = f x
appD :: Name -> [TyVar] -> [Con] -> CG H.Dec
appD nm tyvs _ = do
  let n = length tyvs
      defpats@[fp, _] = [H.VarP $ H.mkName v | v <- ["f", "x"]]
      defbody = H.AppE (H.VarE $ H.mkName "f") (H.VarE $ H.mkName "x")
      defclause = H.Clause defpats (H.NormalB defbody) []

      mxpats = [fp, H.ConP (qmuxnmCG nm) [H.VarP (H.mkName v) | v <- ["p", "a", "b"]]]
      mxbody = foldl1 H.AppE [
                  H.VarE $ H.mkName "Smten.mux0",
                  H.VarE $ H.mkName "p",
                  foldl1 H.AppE [H.VarE (H.mkName v) | v <- ["Smten.strict_app0", "f", "a"]],
                  foldl1 H.AppE [H.VarE (H.mkName v) | v <- ["Smten.strict_app0", "f", "b"]]]
      mxclause = H.Clause mxpats (H.NormalB mxbody) []
  return $ H.FunD (H.mkName $ "strict_app" ++ show n) [mxclause, defclause]
