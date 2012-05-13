
module Seri.Target.Haskell.Builtins.Bool (
    boolB
    ) where

import Seri.Ppr
import Seri.Target.Haskell.Builtin

boolB :: Builtin
boolB =
  let me _ = Nothing
      mt _ = Nothing
  in Builtin {
     mapexp = me,
     maptype = mt,
     includes = text "import Data.Bool(Bool(..))"
  }

