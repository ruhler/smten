
module Smten.CodeGen.Type(typeCG, classCG) where

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
