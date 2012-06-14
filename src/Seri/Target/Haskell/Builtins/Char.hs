
module Seri.Target.Haskell.Builtins.Char (
    charB
  ) where

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H

import Seri.Lambda
import Seri.Target.Haskell.Builtin

charB :: Builtin
charB =
  let me _ = Nothing

      mt (ConT "Char") = Just (H.ConT (H.mkName "Prelude.Char"))
      mt _ = Nothing
  in Builtin {
     mapexp = me,
     maptype = mt,
     includes = H.empty
  }

