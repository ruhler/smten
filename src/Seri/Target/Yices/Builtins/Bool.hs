

module Seri.Target.Yices.Builtins.Bool (
    boolY
  ) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Target.Haskell.Builtin

yIncludes :: [Y.CmdY]
yIncludes = [
    Y.DEFINE ("True?", Y.VarT "(-> bool bool)")
        (Just (Y.LAMBDA [("b", Y.VarT "bool")] (Y.VarE "b"))),
    Y.DEFINE ("False?", Y.VarT "(-> bool bool)")
        (Just (Y.LAMBDA [("b", Y.VarT "bool")] (Y.NOT $ Y.VarE "b")))
    ]

yExp :: Compiler -> Exp -> YCM Y.ExpY
yExp c (ConE (Sig "True" _)) = return $ Y.LitB True
yExp c (ConE (Sig "False" _)) = return $ Y.LitB False
yExp _ _ = fail "boolY doesn't apply"

yType :: Compiler -> Type -> YCM Y.TypY
yType _ (ConT "Bool") = return $ Y.VarT "bool"
yType _ _ = fail "boolY doesn't apply"

boolY :: Compiler
boolY = Compiler yIncludes yExp yType

