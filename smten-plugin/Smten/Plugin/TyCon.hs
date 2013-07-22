
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.TyCon (
    tyconCG,    
  ) where

import Class
import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type
import qualified Smten.Plugin.Output.Syntax as S

-- Declare a type constructor.
-- This works for both normal type constructors and type class constructors.
tyconCG :: TyCon -> CG [S.DataD]
tyconCG t
 | Just cls <- tyConClass_maybe t
 , Just [dc] <- tyConDataCons_maybe t = do
     let mkfield :: Id -> CG S.RecField
         mkfield x = do
           let (vs, mt) = splitForAllTys $ varType x
               vs' = filter (flip notElem (tyConTyVars t)) vs
               mt' = snd $ splitFunTy mt
           t <- topTypeCG $ mkForAllTys vs' mt'
           nm <- nameCG $ varName x
           return $ S.RecField nm t
     fields <- mapM mkfield (classAllSelIds cls)
     cn <- nameCG $ dataConName dc
     t' <- nameCG $ tyConName t
     vs <- mapM (qnameCG . varName) (tyConTyVars t)
     return [S.DataD t' vs [S.RecC cn fields]]
        
 | Just cs <- tyConDataCons_maybe t = do
     let mkcon :: DataCon -> CG S.Con
         mkcon d = do
           tys <- mapM typeCG (dataConOrigArgTys d)
           nm <- nameCG (dataConName d)
           return $ S.Con nm tys
     ks <- mapM mkcon cs
     t' <- nameCG (tyConName t)
     vs <- mapM (qnameCG . varName) (tyConTyVars t)
     return [S.DataD t' vs ks]
 | isSynTyCon t = return []
 | otherwise = do
      lift $ errorMsg (text "Unsupported TyCon in tyconCG: " <+> ppr t)
      return []
  

