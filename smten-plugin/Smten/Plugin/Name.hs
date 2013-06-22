
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
  addimport (moduleName $ nameModule nm)
  return (ppr nm)

