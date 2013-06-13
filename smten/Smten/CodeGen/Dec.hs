
module Smten.CodeGen.Dec (decCG) where

import qualified Language.Haskell.TH as H
import Data.Functor((<$>))

import Smten.Name
import Smten.Type
import Smten.Ppr
import Smten.Dec
import Smten.CodeGen.Annotates
import Smten.CodeGen.CG
import Smten.CodeGen.Data
import Smten.CodeGen.Exp
import Smten.CodeGen.Name
import Smten.CodeGen.Type

decCG :: Dec -> CG [H.Dec]
decCG (DataD _ n tyvars constrs)
  | n == arrowN = return []
  | n `elem` (map fst primdatas) = return []
  | otherwise = dataCG n tyvars constrs

decCG (ClassD _ ctx n vars exps) = do
    (tyvs, ctx') <- contextCG vars ctx
    exps' <- withTyVars vars $ concat <$> mapM topExpCG exps
    return [H.ClassD ctx' (tynameCG n) tyvs [] exps']

decCG (InstD _ ctx cls@(Class n ts) ms) = do
    (_, ctx') <- contextCG cls ctx
    ts' <- mapM typeCG ts
    ms' <- withTyVars cls $ concat <$> mapM (methodCG cls) ms
    let t = foldl H.AppT (H.ConT (qtynameCG n)) ts'
    return [H.InstanceD ctx' t ms']

decCG (ValD _ e@(TopExp (TopSig n _ _) _)) = do
    decs <- topExpCG e
    main <- if unqualified n == name "main"
                 then mainCG n
                 else return []
    return $ decs ++ main

decCG (PrimD _ hsnm ts) = primCG hsnm ts

decCG (AsInHaskellD _ hsmod nm)
 | nm `elem` (map fst primdatas) = return []
 | otherwise = do
    env <- asks cg_env
    DataD _ _ tyv cs <- lookupDataD env nm
    mkHaskellyD (unname hsmod) nm tyv cs

methodCG :: Class -> Method -> CG [H.Dec]
methodCG cls (Method n e) = do
    env <- asks cg_env
    mt <- lookupMethodType env n cls
    mctx <- lookupMethodContext env n cls
    topExpCG (TopExp (TopSig n mctx mt) e)

mainCG :: Name -> CG [H.Dec]
mainCG n = do
  let -- main__ :: Prelude.IO ()
      -- main__ = Smten.stohs main
     sig = H.SigD (H.mkName "main__") (H.AppT (H.ConT $ H.mkName "Prelude.IO")
                                              (H.ConT $ H.mkName "()"))
     body = H.AppE (H.VarE $ H.mkName "Smten.stohs") (H.VarE (qnameCG n))
     clause = H.Clause [] (H.NormalB body) []
     fun = H.FunD (H.mkName "main__") [clause] 
  return [sig, fun]

-- Sample primitive generation:
--   foo :: A -> b -> C
--   foo = frhs (Foo.foohs :: Foo.A -> b -> Foo.C)
--
-- Notes:
--   * haskell types are assumed to have the same names as smten types
--   * haskell types are assumed to be exported by the same module as the
--     haskell primitive
--   * polymorphic types stay polymorphic
--   * It is the user's burden to ensure there are appropriate instances of
--     Haskelly
primCG :: String -> TopSig -> CG [H.Dec]
primCG hsnm ts@(TopSig n ctx t) = do
  sig <- topSigCG ts
  let hsmod = unname . qualification . name $ hsnm

      primty :: Type -> H.Type
      primty (ConT n _)     
        | n == arrowN = H.ArrowT
        | n == listN = H.ListT
        | n == unitN = H.ConT (H.mkName "()")
        | otherwise = H.ConT (H.mkName $ hsmod ++ "." ++ unname (unqualified n))
      primty (AppT a b) = H.AppT (primty a) (primty b)
      primty (VarT n _) = H.VarT $ nameCG n
      primty (NumT i) = H.AppT (H.ConT (H.mkName "Smten.NumT"))
                               (H.LitT (H.NumTyLit i))
      primty (OpT op a b) = foldl1 H.AppT [H.ConT (H.mkName ("(Smten.:" ++ op ++ ":)")), primty a, primty b]
      primty t = error $ "TODO: primty: " ++ pretty t

      body = H.AppE (H.VarE $ H.mkName "Smten.frhs")
                    (H.SigE (H.VarE $ H.mkName hsnm) (primty t))
      fun = H.FunD (nameCG n) [H.Clause [] (H.NormalB body) []]
  return [sig, fun]

