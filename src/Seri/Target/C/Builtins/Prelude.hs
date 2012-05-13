
module Seri.Target.C.Builtins.Prelude (
    preludeB
  ) where

import Seri.Target.C.Builtin
import Seri.Target.C.Builtins.Bool
import Seri.Target.C.Builtins.Integer


preludeB :: Builtin
preludeB = builtins [boolB, integerB]

