
module Smten.CodeGen.Type(typeCG, classCG, topSigCG,) where

import qualified Language.Haskell.TH.Syntax as H

import Smten.Name
import Smten.Type
import Smten.Dec
import Smten.Ppr
import Smten.CodeGen.CG
import Smten.CodeGen.Name

typeCG :: Type -> CG H.Type
typeCG t =
  case t of
    ConT n _
     | n == arrowN -> return H.ArrowT
     | otherwise -> return $ H.ConT (qtynameCG n)
    AppT a b -> do
       a' <- typeCG a
       b' <- typeCG b
       return $ H.AppT a' b'
    VarT n _ -> return $ H.VarT (nameCG n)
    _ -> error $ "todo: typeCG: " ++ pretty t

classCG :: Class -> CG H.Pred
classCG (Class n tys) = do
    tys' <- mapM typeCG tys
    return $ H.ClassP (qtynameCG n) tys'

topSigCG :: TopSig -> CG H.Dec
topSigCG (TopSig nm ctx t) = do
    t' <- typeCG t
    ctx' <- mapM classCG ctx
    tyvnmsbound <- asks cg_tyvars
    let tyvnmsall = map fst (varTs t)
        tyvnmslocal = filter (flip notElem tyvnmsbound) tyvnmsall
        tyvs = map (H.PlainTV . nameCG) tyvnmslocal
    return $ H.SigD (nameCG nm) (H.ForallT tyvs ctx' t')

