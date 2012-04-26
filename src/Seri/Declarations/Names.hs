
module Seri.Declarations.Names (
    valuename, instidname, classname, declassname, methodtypename, tycontypename,
    concretevaluename,
    ) where

import Language.Haskell.TH

import Seri.THUtils

valuename :: Name -> Name
valuename = prefixed "_seriP_"

instidname :: Name -> Name
instidname = prefixed "_seriI_"

classname :: Name -> Name
classname = prefixed "SeriClass_"

declassname :: Name -> Name
declassname n = mkName $ drop (length "SeriClass_") (nameBase n)

methodtypename :: Name -> Name
methodtypename = prefixed "_seriT_"

tycontypename :: Name -> Name
tycontypename = prefixed "_seriS_"

concretevaluename :: Name -> Name
concretevaluename = prefixed "_seriK_"

