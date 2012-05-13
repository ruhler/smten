
module Seri.Target.C.Builtins.Integer (
    integerB
  ) where

import Seri
import qualified Seri.Target.C.AST as C
import Seri.Target.C.Builtin

integerB :: Builtin
integerB =
  let me _ = Nothing

      mt (ConT "Integer") = Just C.intT
      mt _ = Nothing
  in Builtin {
     mapexp = me,
     maptype = mt,
     includes = empty
  }

