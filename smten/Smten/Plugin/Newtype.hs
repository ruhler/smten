
module Smten.Plugin.Newtype (
    newtypeCG,
  ) where

import GhcPlugins

import Data.Functor

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type
import qualified Smten.Plugin.Output.Syntax as S


newtypeCG :: TyCon -> DataCon -> CG [S.Dec]
newtypeCG t constr = do
    let n = tyConName t
        tyvars = tyConTyVars t
    ntD <- newtypeD t constr
    conD <- conD n
    shsD <- smtenHS n tyvars constr
    return $ concat [ntD, conD, shsD]

-- newtype Foo a b ... = Foo {
--   deFoo :: ...
-- }
newtypeD :: TyCon -> DataCon -> CG [S.Dec]
newtypeD t constr = do
    let mkcon :: DataCon -> CG S.Con
        mkcon d = do
          nm <- nameCG (dataConName d)
          fnm <- denewtynmCG (dataConName d)
          [ty] <- mapM topTypeCG (dataConOrigArgTys d)
          return $ S.RecC nm [S.RecField fnm ty]

    nm' <- nameCG (tyConName t)
    vs <- mapM (qnameCG . varName) (tyConTyVars t)
    k <- mkcon constr
    return [S.NewTypeD nm' vs k]

-- __Foo = Foo
conD :: Name -> CG [S.Dec]
conD n = do
  nm <- nameCG n
  cnm <- connmCG n
  return [S.ValD $ S.Val cnm Nothing (S.VarE nm)]
  
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
smtenHS :: Name -> [TyVar] -> DataCon -> CG [S.Dec]
smtenHS nm tyvs constr = do
   let (rkept, rdropped) = span (isStarKind . tyVarKind) (reverse tyvs)
       n = length rkept
       dropped = reverse rdropped
   tyvs' <- mapM tyvarCG dropped
   qtyname <- qtynameCG nm
   ctx <- concat <$> mapM ctxCG dropped
   smtenhsnm <- usequalified "Smten.Runtime.SmtenHS" ("SmtenHS" ++ show n)
   let ty = S.ConAppT smtenhsnm [S.ConAppT qtyname tyvs']
       cn = dataConName constr
   rel <- realizeD cn n
   ite <- iteD cn n
   return [S.InstD ctx ty [rel, ite]]

-- iteN = \p a b = Foo (ite0 p (__deNewTyFoo a) (__deNewTyFoo b))
iteD :: Name -> Int -> CG S.Method
iteD nm k = do
  ite0nm <- usequalified "Smten.Runtime.SmtenHS" "ite0"
  cn <- qnameCG nm
  dcn <- qdenewtynmCG nm
  let ite = foldl1 S.AppE [
        S.VarE ite0nm,
        S.VarE "p",
        S.AppE (S.VarE dcn) (S.VarE "a"),
        S.AppE (S.VarE dcn) (S.VarE "b")]
      foo = S.AppE (S.VarE cn) ite
      body = S.LamE "p" (S.LamE "a" (S.LamE "b" foo))
  return $ S.Method ("ite" ++ show k) body

--   realizeN = \m x -> Foo (realize0 m (__deNewTyFoo x))
realizeD :: Name -> Int -> CG S.Method
realizeD nm k = do
  realize0nm <- usequalified "Smten.Runtime.SmtenHS" "realize0"
  cn <- qnameCG nm
  dcn <- qdenewtynmCG nm
  let rel = foldl1 S.AppE [
                S.VarE realize0nm,
                S.VarE "m",
                S.AppE (S.VarE dcn) (S.VarE "x")]
      foo = S.AppE (S.VarE cn) rel
      body = S.LamE "m" (S.LamE "x" foo)
  return $ S.Method ("realize" ++ show k) body

