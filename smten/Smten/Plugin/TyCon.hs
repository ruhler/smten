
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.TyCon (
    tyconCG,    
  ) where

import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Class
import Smten.Plugin.Data
import Smten.Plugin.Newtype
import qualified Smten.Plugin.Output.Syntax as S

-- Declare a type constructor.
-- This works for both normal type constructors and type class constructors.
tyconCG :: TyCon -> CG [S.Dec]
tyconCG t
 | Just cls <- tyConClass_maybe t
 , Just [dc] <- tyConDataCons_maybe t
 = classCG t cls dc

 | isNewTyCon t
 , Just [constr] <- tyConDataCons_maybe t
 = newtypeCG t constr

 | Just cs <- tyConDataCons_maybe t = dataCG t cs
 | isSynTyCon t = return []
 | otherwise = do
      lift $ errorMsg (text "Unsupported TyCon in tyconCG: " <+> ppr t)
      return []

