
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
    casesD <- concat <$> mapM (mkCaseD n tyvars constrs) constrs
    shsD <- smtenHS n tyvars constrs
    return $ concat [dataD, casesD, shsD]

-- data Foo a b ... = FooA A1 A2 ...
--                  | FooB B1 B2 ...
--                  ...
--                  | FooK K1 K2 ...
--                  | Foo_Prim (Assignment -> Foo a b ...) (Foo a b ...)
--                  | Foo_Ite Bool (Foo a b ...) (Foo a b ...)
--                  | Foo_Error ErrorString
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
      prim = H.NormalC (primnmCG n) [(H.NotStrict, ty) | ty <- [asn, tyme]]

      tybool = H.ConT (qtynameCG boolN)
      ite = H.NormalC (itenmCG n) [(H.NotStrict, ty) | ty <- [tybool, tyme, tyme]]

      errty = H.ConT (H.mkName $ "Smten.ErrorString")
      err = H.NormalC (errnmCG n) [(H.NotStrict, errty)]
  constrs' <- mapM mkcon constrs
  return [H.DataD [] (tynameCG n) tyvars' (constrs' ++ [prim, ite, err]) []]

-- __caseFooX :: Foo a b ... -> (X1 -> X2 -> ... -> z__) -> z__ -> z__
-- __caseFooX x y n =
--    case x of
--      Foo1 {} -> n
--      Foo2 {} -> n
--      ...
--      FooX x1 x2 ... -> y x1 x2 ...
--      ...
--      Foo_Error {} -> sapp (\v -> __caseFooX v y n) x
--      Foo_Prim {} -> sapp (\v -> __caseFooX v y n) x
--      Foo_Ite {} -> sapp (\v -> __caseFooX v y n) x
mkCaseD :: Name -> [TyVar] -> [Con] -> Con -> CG [H.Dec]
mkCaseD n tyvars cs (Con cn tys) = do
  let dt = appsT (conT n) (map tyVarType tyvars)
      zt = varT (name "z__")
      yt = arrowsT (tys ++ [zt])
      ty = arrowsT [dt, yt, zt, zt]
  H.SigD _ ty' <- topSigCG (TopSig (name "DONT_CARE") [] ty)
  let sig = H.SigD (casenmCG cn) ty'

      [vv, vx, vy, vn] = map H.mkName ["v", "x", "y", "n"]
      vxs = [H.mkName ("x" ++ show i) | i <- [1..(length tys)]]

      mkcon :: Con -> H.Match
      mkcon (Con n _) 
        | n == cn = H.Match (H.ConP (qnameCG cn) (map H.VarP vxs))
                       (H.NormalB (foldl H.AppE (H.VarE vy) (map H.VarE vxs))) []
        | otherwise = H.Match (H.RecP (qnameCG n) []) (H.NormalB (H.VarE vn)) []

      sappbody = H.NormalB $ foldl1 H.AppE [
        H.VarE $ H.mkName "Smten.sapp",
        H.LamE [H.VarP vv] (foldl1 H.AppE (map H.VarE [qcasenmCG cn, vv, vy, vn])),
        H.VarE vx]

      mkerr = H.Match (H.RecP (qerrnmCG n) []) sappbody []
      mkprim = H.Match (H.RecP (qprimnmCG n) []) sappbody []
      mkite = H.Match (H.RecP (qitenmCG n) []) sappbody []

      matches = map mkcon cs ++ [mkerr, mkprim, mkite]
      cse = H.CaseE (H.VarE vx) matches
      clause = H.Clause (map H.VarP [vx, vy, vn]) (H.NormalB cse) []
      fun = H.FunD (casenmCG cn) [clause]
  return [sig, fun]

-- instance (Haskelly ha sa, Haskelly hb sb, ...) =>
--   Haskelly (Foo ha hb ...) (Smten.Lib.Foo sa sb ...) where
--     frhs ...
--     tohs ...
mkHaskellyD :: String -> Name -> [TyVar] -> [Con] -> CG [H.Dec]
mkHaskellyD hsmod nm tyvars cons = do
  let hvars = [H.VarT (H.mkName $ "h" ++ unname n) | TyVar n _ <- tyvars]
      svars = [H.VarT (H.mkName $ "s" ++ unname n) | TyVar n _ <- tyvars]
      ctx = [H.ClassP (H.mkName "Smten.Haskelly") [ht, st] | (ht, st) <- zip hvars svars]
      ht = foldl H.AppT (H.ConT (qhstynameCG hsmod nm)) hvars
      st = foldl H.AppT (H.ConT (qtynameCG nm)) svars
      ty = foldl1 H.AppT [H.ConT $ H.mkName "Smten.Haskelly", ht, st]
  frhs <- mkFrhsD hsmod cons
  tohs <- mkTohsD hsmod cons
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

--     tohs (Smten.Lib.FooA x1 x2 ...) = FooA (tohs x1) (tohs x2) ...
--     tohs (Smten.Lib.FooB x1 x2 ...) = FooB (tohs xs) (tohs x2) ...
--     ...
mkTohsD :: String -> [Con] -> CG H.Dec
mkTohsD hsmod cons = do
  let mkcon :: Con -> H.Clause
      mkcon (Con cn tys) = 
        let xs = [H.mkName $ "x" ++ show i | i <- [1..(length tys)]]
            pat = H.ConP (qnameCG cn) (map H.VarP xs)
            body = foldl H.AppE (H.ConE (qhsnameCG hsmod cn)) [H.AppE (H.VarE $ H.mkName "Smten.tohs") (H.VarE x) | x <- xs]
        in H.Clause [pat] (H.NormalB body) []
  return $ H.FunD (H.mkName "tohs") (map mkcon cons)

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
smtenHS :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
smtenHS nm tyvs cs = do
   let (rkept, rdropped) = span (\(TyVar n k) -> knum k == 0) (reverse tyvs)
       n = length rkept
       dropped = reverse rdropped
       ctx = [H.ClassP (H.mkName $ "Smten.SmtenHS" ++ show (knum k)) [H.VarT (nameCG n)] | TyVar n k <- dropped]
       ty = H.AppT (H.VarT (H.mkName $ "Smten.SmtenHS" ++ show n))
                   (foldl H.AppT (H.ConT $ qtynameCG nm) [H.VarT (nameCG n) | TyVar n _ <- dropped])
   rel <- realizeD nm n cs
   prim <- primD nm n
   ite <- iteD nm n
   err <- errorD nm n
   sapp <- sappD nm n
   return [H.InstanceD ctx ty [rel, ite, prim, err, sapp]]

--   primN = Foo_Prim
primD :: Name -> Int -> CG H.Dec
primD nm n = do
  let body = H.ConE (qprimnmCG nm)
      clause = H.Clause [] (H.NormalB body) []
      fun = H.FunD (H.mkName $ "primitive" ++ show n) [clause]
  return fun

--   iteN = Foo_Ite
iteD :: Name -> Int -> CG H.Dec
iteD nm n = do
  let body = H.NormalB $ H.VarE (qitenmCG nm)
      fun = H.ValD (H.VarP (H.mkName $ "ite" ++ show n)) body []
  return fun

--   errorN = Foo_Error
errorD :: Name -> Int -> CG H.Dec
errorD nm n = do
  let body = H.NormalB $ H.VarE (qerrnmCG nm)
      fun = H.ValD (H.VarP (H.mkName $ "error" ++ show n)) body []
  return fun

--   realizeN m (FooA x1 x2 ...) = FooA (realize m x1) (realize m x2) ...
--   realizeN m (FooB x1 x2 ...) = FooB (realize m x1) (realize m x2) ...
--   ...
--   realizeN m (Foo_Prim r _) = r m
--   realizeN m (Foo_Ite p a b) = iterealize m p a b
--   realizeN m x@(Foo_Error _) = x
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

      primpats = [H.VarP $ H.mkName "m", H.ConP (qprimnmCG n) [H.VarP (H.mkName "r"), H.WildP]]
      primbody = H.AppE (H.VarE (H.mkName "r")) (H.VarE (H.mkName "m"))
      primcon = H.Clause primpats (H.NormalB primbody) []

      itepats = [H.VarP $ H.mkName "m", H.ConP (qitenmCG n) (map (H.VarP . H.mkName) ["p", "a", "b"])]
      itebody = foldl1 H.AppE (map (H.VarE . H.mkName) ["Smten.iterealize", "m", "p", "a", "b"])
      itecon = H.Clause itepats (H.NormalB itebody) []

      errpats = [H.VarP $ H.mkName "m", H.AsP (H.mkName "x") (H.ConP (qerrnmCG n) [H.WildP])]
      errbody = H.VarE $ H.mkName "x"
      errcon = H.Clause errpats (H.NormalB errbody) []
  return $ H.FunD (H.mkName $ "realize" ++ show k) (map mkcon cs ++ [primcon, itecon, errcon])

--   sappN f (Foo_Ite p a b) = itesapp f p a b
--   sappN f (Foo_Error msg) = error0 msg
--   sappN f (Foo_Prim r c) = primsapp f r c
--   sappN f x = f x
sappD :: Name -> Int -> CG H.Dec
sappD n k = do
  let primpats = [H.VarP $ H.mkName "f", H.ConP (qprimnmCG n) [H.VarP (H.mkName "r"), H.VarP (H.mkName "c")]]
      primbody = foldl1 H.AppE [
                    H.VarE $ H.mkName "Smten.primsapp",
                    H.VarE $ H.mkName "f",
                    H.VarE $ H.mkName "r",
                    H.VarE $ H.mkName "c"]
      primcon = H.Clause primpats (H.NormalB primbody) []

      itepats = [H.VarP $ H.mkName "f", H.ConP (qitenmCG n) (map (H.VarP . H.mkName) ["p", "a", "b"])]
      itebody = foldl1 H.AppE (map (H.VarE . H.mkName) ["Smten.itesapp", "f", "p", "a", "b"])
      itecon = H.Clause itepats (H.NormalB itebody) []

      errpats = [H.VarP $ H.mkName "f", H.ConP (qerrnmCG n) [H.VarP $ H.mkName "msg"]]
      errbody = H.AppE (H.VarE $ H.mkName "Smten.error0") (H.VarE $ H.mkName "msg")
      errcon = H.Clause errpats (H.NormalB errbody) []

      defpats = [H.VarP $ H.mkName "f", H.VarP $ H.mkName "x"]
      defbody = H.AppE (H.VarE $ H.mkName "f") (H.VarE $ H.mkName "x")
      defcon = H.Clause defpats (H.NormalB defbody) []
  return $ H.FunD (H.mkName $ "sapp" ++ show k) [itecon, errcon, primcon, defcon]

