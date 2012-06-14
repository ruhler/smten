
module Seri.Target.Haskell.Builtins.Integer (
    integerB
  ) where

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H

import Seri.Lambda
import Seri.Target.Haskell.Builtin

integerB :: Builtin
integerB =
  let me (PrimE (Sig "__prim_add_Integer" _)) = Just (H.VarE $ H.mkName "Integer.+")
      me (PrimE (Sig "__prim_sub_Integer" _)) = Just (H.VarE $ H.mkName "Integer.-")
      me (PrimE (Sig "__prim_mul_Integer" _)) = Just (H.VarE $ H.mkName "Integer.*")
      me (PrimE (Sig "<" _)) = Just (H.VarE $ H.mkName "Integer.<")
      me (PrimE (Sig ">" _)) = Just (H.VarE $ H.mkName "Integer.>")
      me (PrimE (Sig "==" _)) = Just (H.VarE $ H.mkName "Integer.==")
      me _ = Nothing

      mt (ConT "Integer") = Just (H.ConT $ H.mkName "Integer.Integer")
      mt _ = Nothing
  in Builtin {
     mapexp = me,
     maptype = mt,
     includes = H.text "import qualified Seri.Target.Haskell.Lib.Integer as Integer"
  }

