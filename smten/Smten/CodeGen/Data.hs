
module Smten.CodeGen.Data (
    dataCG, mkHaskellyD,
    ) where

import qualified Language.Haskell.TH.Syntax as H

import Data.Functor((<$>))

import Smten.Name
import Smten.Type
import Smten.Dec
import Smten.CodeGen.CG
import Smten.CodeGen.Type
import Smten.CodeGen.Name

dataCG :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
dataCG n tyvars constrs = do
    dataD <- mkDataD n tyvars constrs
    casesD <- concat <$> mapM (mkCaseD n tyvars) constrs
    shsD <- smtenHS n tyvars constrs
    return $ concat [dataD, casesD, shsD]

-- data Foo a b ... = FooA A1 A2 ...
--                  | FooB B1 B2 ...
--                  ...
--                  | FooK K1 K2 ...
--                  | Foo_Prim (Assignment -> Foo a b ...) (Cases (Foo a b ...))
mkDataD :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
mkDataD n tyvars constrs = do
  let tyvars' = [H.PlainTV (nameCG nm) | TyVar nm _ <- tyvars]

      mkcon :: Con -> CG H.Con
      mkcon (Con cn tys) = do
        let cn' = nameCG cn
        tys' <- mapM typeCG tys
        return (H.NormalC cn' [(H.NotStrict, ty') | ty' <- tys'])

      tyme = foldl H.AppT (H.ConT (qtynameCG n)) [H.VarT (nameCG nm) | TyVar nm _ <- tyvars]
      asn = foldl H.AppT H.ArrowT [H.ConT (H.mkName "Smten.Assignment"), tyme]
      css = H.AppT (H.ConT (H.mkName "Smten.Cases")) tyme
      prim = H.NormalC (primnmCG n) [(H.NotStrict, ty) | ty <- [asn, css]]
  constrs' <- mapM mkcon constrs
  return [H.DataD [] (tynameCG n) tyvars' (constrs' ++ [prim]) []]

-- __caseFooX :: Foo a b ... -> (X1 -> X2 -> ... -> z__) -> z__ -> z__
-- __caseFooX x y n =
--    case x of
--      FooX x1 x2 ... -> y x1 x2 ...
--      Foo_Prim _ _ -> prim3 __caseFooX x y n
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

      nms = [H.mkName "Smten.prim3", qcasenmCG cn, vx, vy, vn]
      mbody = foldl1 H.AppE [H.VarE n | n <- nms]
      matchm = H.Match (H.ConP (qprimnmCG n) [H.WildP, H.WildP]) (H.NormalB mbody) []

      matchn = H.Match H.WildP (H.NormalB (H.VarE vn)) []
      cse = H.CaseE (H.VarE vx) [matchy, matchm, matchn]

      clause = H.Clause (map H.VarP [vx, vy, vn]) (H.NormalB cse) []
      fun = H.FunD (casenmCG cn) [clause]
  return [sig, fun]

-- instance (Haskelly ha sa, Haskelly hb sb, ...) =>
--   Haskelly (Foo ha hb ...) (Smten.Lib.Foo sa sb ...) where
--     frhs ...
--     mtohs ...
mkHaskellyD :: String -> Name -> [TyVar] -> [Con] -> CG [H.Dec]
mkHaskellyD hsmod nm tyvars cons = do
  let hvars = [H.VarT (H.mkName $ "h" ++ unname n) | TyVar n _ <- tyvars]
      svars = [H.VarT (H.mkName $ "s" ++ unname n) | TyVar n _ <- tyvars]
      ctx = [H.ClassP (H.mkName "Smten.Haskelly") [ht, st] | (ht, st) <- zip hvars svars]
      ht = foldl H.AppT (H.ConT (qhstynameCG hsmod nm)) hvars
      st = foldl H.AppT (H.ConT (qtynameCG nm)) svars
      ty = foldl1 H.AppT [H.ConT $ H.mkName "Smten.Haskelly", ht, st]
  frhs <- mkFrhsD hsmod cons
  tohs <- mkTohsD hsmod nm cons
  return [H.InstanceD ctx ty [frhs, tohs]]

--     frhs (FooA x1 x2 ...) = Smten.Lib.FooA (frhs x1) (frhs x2) ...
--     frhs (FooB x1 x2 ...) = Smten.Lib.FooB (frhs xs) (frhs x2) ...
--     ...
mkFrhsD :: String -> [Con] -> CG H.Dec
mkFrhsD hsmod cons = do
  let mkcon :: Con -> H.Clause
      mkcon (Con cn tys) = 
        let xs = [H.mkName $ "x" ++ show i | i <- [1..(length tys)]]
            pat = H.ConP (qhsnameCG hsmod cn) (map H.VarP xs)
            body = foldl H.AppE (H.ConE (qnameCG cn)) [H.AppE (H.VarE $ H.mkName "Smten.frhs") (H.VarE x) | x <- xs]
        in H.Clause [pat] (H.NormalB body) []
  return $ H.FunD (H.mkName "frhs") (map mkcon cons)

--     mtohs (Smten.Lib.FooA x1 x2 ...) = do
--        x1' <- mtohs x1
--        x2' <- mtohs x2
--        ...
--        return (FooA x1' x2' ...)
--     mtohs (Smten.Lib.FooB x1 x2 ...) = do
--        x1' <- mtohs x1
--        x2' <- mtohs x2
--        ...
--        return (FooB x1' x2' ...)
 --     ...
---     mtohs _ = Nothing
mkTohsD :: String -> Name -> [Con] -> CG H.Dec
mkTohsD hsmod nm cons = do
  let mkcon :: Con -> H.Clause
      mkcon (Con cn tys) = 
        let xs = [H.mkName $ "x" ++ show i | i <- [1..(length tys)]]
            xs' = [H.mkName $ "x" ++ show i ++ "'" | i <- [1..(length tys)]]
            pat = H.ConP (qnameCG cn) (map H.VarP xs)
            stmts = [H.BindS (H.VarP x') (H.AppE (H.VarE (H.mkName "Smten.mtohs")) (H.VarE x)) | (x, x') <- zip xs xs']
            rtn = H.NoBindS $ H.AppE (H.VarE (H.mkName "Prelude.return"))
                                     (foldl H.AppE (H.ConE (qhsnameCG hsmod cn)) (map H.VarE xs'))
            body = H.DoE (stmts ++ [rtn])
        in H.Clause [pat] (H.NormalB body) []
      def = H.Clause [H.WildP] (H.NormalB (H.VarE (H.mkName "Prelude.Nothing"))) []
  return $ H.FunD (H.mkName "mtohs") (map mkcon cons ++ [def])

-- Note: we currently don't support crazy kinded instances of SmtenHS. This
-- means we are limited to "linear" kinds of the form (* -> * -> ... -> *)
--
-- To handle that properly, we chop off as many type variables as needed to
-- get to a linear kind.
--   call the chopped off type variables c1, c2, ...
--
-- instance (SmtenN c1, SmtenN c2, ...) => SmtenHSN (Foo c1 c2 ...) where
--   realizeN = ...
--   casesN = ...
--   primitiveN = ...
smtenHS :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
smtenHS nm tyvs cs = do
   let (rkept, rdropped) = span (\(TyVar n k) -> knum k == 0) (reverse tyvs)
       n = length rkept
       dropped = reverse rdropped
       ctx = [H.ClassP (H.mkName $ "Smten.SmtenHS" ++ show (knum k)) [H.VarT (nameCG n)] | TyVar n k <- dropped]
       ty = H.AppT (H.VarT (H.mkName $ "Smten.SmtenHS" ++ show n))
                   (foldl H.AppT (H.ConT $ qtynameCG nm) [H.VarT (nameCG n) | TyVar n _ <- dropped])
   rel <- realizeD nm n cs
   cases <- casesD nm n cs
   prim <- primD nm n
   return [H.InstanceD ctx ty [rel, cases, prim]]

--   primN = Foo_Prim
primD :: Name -> Int -> CG H.Dec
primD nm n = do
  let body = H.NormalB $ H.VarE (qprimnmCG nm)
      fun = H.ValD (H.VarP (H.mkName $ "primitive" ++ show n)) body []
  return fun

--   realizeN m (FooA x1 x2 ...) = FooA (realize m x1) (realize m x2) ...
--   realizeN m (FooB x1 x2 ...) = FooB (realize m x1) (realize m x2) ...
--   ...
--   realizeN m (Foo_Prim r _) = r m
realizeD :: Name -> Int -> [Con] -> CG H.Dec
realizeD n k cs = do
  let mkcon :: Con -> H.Clause
      mkcon (Con cn cts) =
        let xs = [H.mkName $ "x" ++ show i | i <- [1..length cts]]
            pats = [H.VarP $ H.mkName "m", H.ConP (qnameCG cn) (map H.VarP xs)]
            rs = [foldl1 H.AppE [
                    H.VarE (H.mkName "Smten.realize"),
                    H.VarE (H.mkName "m"),
                    H.VarE x] | x <- xs]
            body = foldl H.AppE (H.ConE (qnameCG cn)) rs
        in H.Clause pats (H.NormalB body) []

      mxpats = [H.VarP $ H.mkName "m", H.ConP (qprimnmCG n) [H.VarP (H.mkName "r"), H.WildP]]
      mxbody = H.AppE (H.VarE (H.mkName "r")) (H.VarE (H.mkName "m"))
      mxcon = H.Clause mxpats (H.NormalB mxbody) []
  return $ H.FunD (H.mkName $ "realize" ++ show k) (map mkcon cs ++ [mxcon])

-- casesN (Foo_Prim _ c) = c
-- casesN x = concrete x
casesD :: Name -> Int -> [Con] -> CG H.Dec
casesD nm n _ = do
  let defbody = H.AppE (H.VarE $ H.mkName "Smten.concrete") (H.VarE $ H.mkName "x")
      defclause = H.Clause [H.VarP $ H.mkName "x"] (H.NormalB defbody) []

      mxpats = [H.ConP (qprimnmCG nm) [H.WildP, H.VarP (H.mkName "c")]]
      mxbody = H.VarE (H.mkName "c")
      mxclause = H.Clause mxpats (H.NormalB mxbody) []
  return $ H.FunD (H.mkName $ "cases" ++ show n) [mxclause, defclause]

