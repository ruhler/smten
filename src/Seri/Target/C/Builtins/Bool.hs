
module Seri.Target.C.Builtins.Bool (
    boolB
    ) where

import Seri
import Seri.Utils.Ppr
import Seri.Target.C.AST as C
import Seri.Target.C.Builtin

boolB :: Builtin
boolB =
  let me _ = Nothing

      mt (ConT "Bool") = Just C.boolT
      mt _ = Nothing
  in Builtin {
     mapexp = me,
     maptype = mt,
     includes = text "int True(void) { return 1; }"
            $+$ text "int False(void) { return 0; }"
  }

