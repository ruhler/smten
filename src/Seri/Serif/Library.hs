
module Seri.Serif.Library where

import Seri.FrontEnd.Typed
import Seri.Lambda

instance SeriType Integer where
  seritype _ = ConT "Integer"

_seriS_Integer :: Type
_seriS_Integer = ConT "Integer"

_seriK_Integer :: Integer
_seriK_Integer = undefined


instance SeriType Bool where
  seritype _ = ConT "Bool"

_seriS_Bool :: Type
_seriS_Bool = ConT "Bool"

_seriK_Bool :: Bool
_seriK_Bool = undefined

_seriP_True :: Typed Exp Bool
_seriP_True = conE' "True"

_seriP_False :: Typed Exp Bool
_seriP_False = conE' "False"

