
module Seri.Target.Haskell.Builtins.Char (
    charB
  ) where

import Seri
import Seri.Target.Haskell.Builtin

import qualified Language.Haskell.TH as H

charB :: Builtin
charB =
  let me _ = Nothing

      mt (ConT "Char") = Just (H.ConT (H.mkName "Prelude.Char"))
      mt _ = Nothing
  in Builtin {
     mapexp = me,
     maptype = mt,
     includes = empty
  }

