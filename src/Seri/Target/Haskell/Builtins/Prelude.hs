
module Seri.Target.Haskell.Builtins.Prelude (
    preludeH
  ) where

import qualified Language.Haskell.TH as H

import Seri.Target.Haskell.Compiler
import Seri.Target.Haskell.Builtins.Integer

import Seri.Lambda

preludeH :: HCompiler
preludeH =
  let me _ e = fail $ "preludeH does not apply to exp: " ++ pretty e

      mt _ (ConT "Char") = return $ H.ConT (H.mkName "Prelude.Char")
      mt _ t = fail $ "preludeH does not apply to type: " ++ pretty t

      md _ (PrimD (TopSig "error" _ t)) = do
        let e = H.VarE (H.mkName "Prelude.error")
        let val = H.FunD (H.mkName "error") [H.Clause [] (H.NormalB e) []]
        return [val]
      md _ (DataD "Char" _ _) = return []
      md _ (DataD "Integer" _ _) = return []
      md _ (DataD "()" _ _) = return []
      md _ (DataD "(,)" _ _) = return []
      md _ (DataD "(,,)" _ _) = return []
      md _ (DataD "(,,,)" _ _) = return []
      md _ (DataD "[]" _ _) = return []
      md _ d = fail $ "preludeH does not apply to dec: " ++ pretty d
  in compilers [Compiler me mt md, integerH]

