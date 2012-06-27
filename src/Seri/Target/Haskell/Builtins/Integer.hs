
module Seri.Target.Haskell.Builtins.Integer (
    integerH
  ) where

import Data.Char(isAlphaNum)

import qualified Language.Haskell.TH as H

import Seri.Failable
import Seri.Lambda
import Seri.Target.Haskell.Compiler

integerH :: HCompiler
integerH =
  let me _ e = fail $ "integerH does not apply to exp: " ++ pretty e

      mt _ (ConT "Integer") = return $ H.ConT (H.mkName "Prelude.Integer")
      mt _ t = fail $ "integerH does not apply to type: " ++ pretty t

      issymbol :: Name -> Bool
      issymbol (h:_) = not $ isAlphaNum h || h == '_'

      iprim :: HCompiler -> TopSig -> H.Exp -> Failable [H.Dec]
      iprim c (TopSig nm _ t) b = do
        t' <- compile_type c c t
        let hsn = H.mkName $ if issymbol nm then "(" ++ nm ++ ")" else nm
        let sig = H.SigD hsn t'
        let val = H.FunD hsn [H.Clause [] (H.NormalB b) []]
        return [sig, val]

      vare :: String -> H.Exp
      vare n = H.VarE (H.mkName n)

      bprim :: HCompiler -> TopSig -> String -> Failable [H.Dec]
      bprim c (TopSig nm _ t) b = do    
        t' <- compile_type c c t
        let hsn = H.mkName $ if issymbol nm then "(" ++ nm ++ ")" else nm
        let sig = H.SigD hsn t'
        let val = H.FunD hsn [H.Clause
                [H.VarP (H.mkName "a"), H.VarP (H.mkName "b")]
                    (H.NormalB (
                        H.CondE (H.AppE (H.AppE (vare b) (vare "a")) (vare "b"))
                                (H.ConE (H.mkName "True"))
                                (H.ConE (H.mkName "False"))
                    )) []]
        return [sig, val]

      md c (PrimD s@(TopSig "__prim_add_Integer" _ _)) = iprim c s (vare "Prelude.+")
      md c (PrimD s@(TopSig "__prim_sub_Integer" _ _)) = iprim c s (vare "Prelude.-")
      md c (PrimD s@(TopSig "__prim_mul_Integer" _ _)) = iprim c s (vare "Prelude.*")
      md c (PrimD s@(TopSig "<" _ _)) = bprim c s "Prelude.<"
      md c (PrimD s@(TopSig ">" _ _)) = bprim c s "Prelude.>"
      md c (PrimD s@(TopSig "__prim_eq_Integer" _ _)) = bprim c s "Prelude.=="
      md _ d = fail $ "integerH does not apply to dec: " ++ pretty d
  in Compiler me mt md

