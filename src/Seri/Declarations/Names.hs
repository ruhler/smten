
module Seri.Declarations.Names (
    valuename, instidname, classname, methodtypename,
    ) where

import Language.Haskell.TH

import Seri.THUtils

valuename :: Name -> Name
valuename = prefixed "_seriP_"

instidname :: Name -> Name
instidname = prefixed "_seriI_"

classname :: Name -> Name
classname = prefixed "SeriClass_"

methodtypename :: Name -> Name
methodtypename = prefixed "_seriT_"

