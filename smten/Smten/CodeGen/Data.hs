
module Smten.CodeGen.Data (dataCG) where

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
    return $ concat [dataD, casesD]

-- data Foo a b ... = FooA A1 A2 ...
--                  | FooB B1 B2 ...
--                  ...
--                  | FooK K1 K2 ...
mkDataD :: Name -> [TyVar] -> [Con] -> CG [H.Dec]
mkDataD n tyvars constrs = do
  let tyvars' = [H.PlainTV (nameCG nm) | TyVar nm _ <- tyvars]

      mkcon :: Con -> CG H.Con
      mkcon (Con cn tys) = do
        let cn' = nameCG cn
        tys' <- mapM typeCG tys
        return (H.NormalC cn' [(H.NotStrict , ty') | ty' <- tys'])
  constrs' <- mapM mkcon constrs
  return [H.DataD [] (tynameCG n) tyvars' constrs' []]

-- __caseFooX :: Foo a b ... -> (X1 -> X2 -> ... -> z__) -> z__ -> z__
-- __caseFooX x y n =
--    case x of
--      FooX x1 x2 ... -> y x1 x2 ...
--      _ -> n
mkCaseD :: Name -> [TyVar] -> Con -> CG [H.Dec]
mkCaseD n tyvars (Con cn tys) = do
  let dt = appsT (conT n) (map tyVarType tyvars)
      zt = varT (name "z__")
      yt = arrowsT (tys ++ [zt])
      ty = arrowsT [dt, yt, zt, zt]
  ty' <- typeCG ty
  let sig = H.SigD (casenmCG cn) ty'

      [vx, vy, vn] = map H.mkName ["x", "y", "n"]
      vxs = [H.mkName ("x" ++ show i) | i <- [1..(length tys -1)]]
      matchy = H.Match (H.ConP (qnameCG cn) (map H.VarP vxs))
                       (H.NormalB (foldl H.AppE (H.VarE vy) (map H.VarE vxs))) []
      matchn = H.Match H.WildP (H.NormalB (H.VarE vn)) []
      cse = H.CaseE (H.VarE vx) [matchy, matchn]

      clause = H.Clause (map H.VarP [vx, vy, vn]) (H.NormalB cse) []
      fun = H.FunD (casenmCG cn) [clause]
  return [sig, fun]

