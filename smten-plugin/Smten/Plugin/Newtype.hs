
module Smten.Plugin.Newtype (
    newtypeCG,
  ) where

import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type
import qualified Smten.Plugin.Output.Syntax as S

newtypeCG :: TyCon -> DataCon -> CG [S.Dec]
newtypeCG t constr = do
    let mkcon :: DataCon -> CG S.Con
        mkcon d = do
          nm <- nameCG (dataConName d)
          tys <- mapM typeCG (dataConOrigArgTys d)
          return $ S.Con nm tys

    nm' <- nameCG (tyConName t)
    vs <- mapM (qnameCG . varName) (tyConTyVars t)
    k <- mkcon constr
    addimport "Smten.Runtime.SmtenHS"
    return [S.NewTypeD nm' vs k ["Smten.Runtime.SmtenHS.SmtenHS" ++ show (length vs)]]

