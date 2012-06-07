

module Seri.Target.Yices.Builtins.Bool (
    boolY
  ) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Utils.Ppr
import Seri.Target.Haskell.Builtin

yExp :: Compiler -> Exp -> Maybe Y.ExpY
yExp c (ConE (Sig "True" _)) = Just $ Y.LitB True
yExp c (ConE (Sig "False" _)) = Just $ Y.LitB False
yExp _ _ = Nothing

yType :: Compiler -> Type -> Maybe Y.TypY
yType _ (ConT "Bool") = Just $ Y.VarT "bool"
yType _ _ = Nothing

boolY :: Compiler
boolY = Compiler yExp yType

