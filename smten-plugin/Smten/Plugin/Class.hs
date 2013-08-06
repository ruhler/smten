
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
      -- Generate a newtype for this class, because it only has a single
      -- field. We need to generate both the method, and the newtype
      -- de-constructor.
      let (vs, mt) = splitForAllTys $ varType fld
          vs' = filter (flip notElem (tyConTyVars t)) vs
          mt' = snd $ splitFunTy mt
      ft <- topTypeCG $ mkForAllTys vs' mt'
      dnm <- denewtynmCG $ dataConName dc
      let denew = S.RecField dnm ft

      cn <- nameCG $ dataConName dc
      t' <- nameCG $ tyConName t
      vs <- mapM (qnameCG . varName) (tyConTyVars t)
      let nty = S.NewTypeD t' vs (S.RecC cn [denew])

      flt <- topTypeCG $ varType fld
      flnm <- nameCG $ varName fld
      let fldf = S.ValD (S.Val flnm (Just flt) (S.VarE dnm))
      return [nty, fldf]

  | otherwise = do
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
      return [S.DataD (S.Data t' vs [S.RecC cn fields])]



