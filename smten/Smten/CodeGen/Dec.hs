
module Smten.CodeGen.Dec (decCG) where

import qualified Language.Haskell.TH as H
import Data.Functor((<$>))

import Smten.Name
import Smten.Dec
import Smten.CodeGen.CG
import Smten.CodeGen.Data
import Smten.CodeGen.Exp
import Smten.CodeGen.Name
import Smten.CodeGen.Type

decCG :: Dec -> CG [H.Dec]
decCG (DataD _ n tyvars constrs)
  | n == arrowN = return []
  | otherwise = dataCG n tyvars constrs
decCG (ClassD _ ctx n vars exps) = do
    ctx' <- mapM classCG ctx
    let vars' = map (H.PlainTV . nameCG . tyVarName) vars
    exps' <- concat <$> mapM topExpCG exps
    return [H.ClassD ctx' (tynameCG n) vars' [] exps']
decCG (InstD _ ctx cls@(Class n ts) ms) = do
    ctx' <- mapM classCG ctx
    ms' <- concat <$> mapM (methodCG cls) ms
    ts' <- mapM typeCG ts
    let t = foldl H.AppT (H.ConT (qtynameCG n)) ts'
    return [H.InstanceD ctx' t ms']
decCG (ValD _ e) = topExpCG e
decCG (PrimD {}) = return []

methodCG :: Class -> Method -> CG [H.Dec]
methodCG cls (Method n e) = do
    env <- asks cg_env
    mt <- lookupMethodType env n cls
    mctx <- lookupMethodContext env n cls
    topExpCG (TopExp (TopSig n mctx mt) e)
