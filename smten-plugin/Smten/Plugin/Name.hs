
module Smten.Plugin.Name (
    nameCG, qnameCG,
    ) where

import GhcPlugins
import Smten.Plugin.CG

-- Generate code for an unqualified name.
nameCG :: Name -> CG SDoc
nameCG nm = return (ppr $ localiseName nm)

-- Generate code for a qualified name.
qnameCG :: Name -> CG SDoc
qnameCG nm = do
  case (nameModule_maybe nm) of
     Just mn -> addimport (moduleName mn)
     _ -> return ()
  return (ppr nm)

