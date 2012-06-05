
module Seri.Target.Haskell.Builtins.Integer (
    integerB
  ) where

import qualified Language.Haskell.TH as H

import Seri.Lambda
import Seri.Utils.Ppr
import Seri.Target.Haskell.Builtin

integerB :: Builtin
integerB =
  let me (PrimE (Sig "+" _)) = Just (H.VarE $ H.mkName "Integer.+")
      me (PrimE (Sig "-" _)) = Just (H.VarE $ H.mkName "Integer.-")
      me (PrimE (Sig "*" _)) = Just (H.VarE $ H.mkName "Integer.*")
      me (PrimE (Sig "<" _)) = Just (H.VarE $ H.mkName "Integer.<")
      me (PrimE (Sig ">" _)) = Just (H.VarE $ H.mkName "Integer.>")
      me (PrimE (Sig "==" _)) = Just (H.VarE $ H.mkName "Integer.==")
      me _ = Nothing

      mt (ConT "Integer") = Just (H.ConT $ H.mkName "Integer.Integer")
      mt _ = Nothing
  in Builtin {
     mapexp = me,
     maptype = mt,
     includes = text "import qualified Seri.Target.Haskell.Lib.Integer as Integer"
  }

