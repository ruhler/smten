
{-# LANGUAGE PatternGuards #-}
module Smten.Plugin.Class (
   classCG, 
    ) where

import Class
import GhcPlugins
import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type
import qualified Smten.Plugin.Output.Syntax as S

-- Generate code for a type constructor representing a class.
classCG :: TyCon -> Class -> DataCon -> CG [S.Dec]
classCG t cls dc
  | [fld] <- classAllSelIds cls = do
      -- Generate a newtype for this class, because it only has a single field.
      let (vs, mt) = splitForAllTys $ varType fld
          vs' = filter (flip notElem (tyConTyVars t)) vs
          mt' = snd $ splitFunTy mt
      ft <- topTypeCG $ mkForAllTys vs' mt'
      dnm <- denewtynmCG $ dataConName dc
      let denew = S.RecField dnm ft

      cn <- nameCG $ dataConName dc
      t' <- nameCG $ tyConName t
      vs <- mapM (qnameCG . varName) (tyConTyVars t)
      addexport (S.TyConExport t')
      return [S.NewTypeD t' vs (S.RecC cn [denew])]

  | otherwise = do
      let mkfield :: Id -> CG S.Type
          mkfield x = do
            let (vs, mt) = splitForAllTys $ varType x
                vs' = filter (flip notElem (tyConTyVars t)) vs
                mt' = snd $ splitFunTy mt
            topTypeCG $ mkForAllTys vs' mt'
      fields <- mapM mkfield (classAllSelIds cls)
      cn <- nameCG $ dataConName dc
      t' <- nameCG $ tyConName t
      vs <- mapM (qnameCG . varName) (tyConTyVars t)
      addexport (S.TyConExport t')
      return [S.DataD (S.Data t' vs [S.Con cn fields])]



