
module Seri.Target.Haskell.Builtins.Prelude (
    preludeB
  ) where

import Seri.Target.Haskell.Builtin
import Seri.Target.Haskell.Builtins.Bool
import Seri.Target.Haskell.Builtins.Char
import Seri.Target.Haskell.Builtins.Integer


preludeB :: Builtin
preludeB = builtins [boolB, charB, integerB]

