
module Seri.Target.Yices.Builtins.Integer (
    integerY
  ) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Utils.Ppr
import Seri.Target.Haskell.Builtin

yExp :: Compiler -> Exp -> Maybe Y.ExpY
yExp c (AppE (AppE (PrimE (Sig "<" _)) a) b) = do
    a' <- compile_exp c c a
    b' <- compile_exp c c b
    return (a' Y.:< b')
yExp c (AppE (AppE (PrimE (Sig ">" _)) a) b) = do
    a' <- compile_exp c c a
    b' <- compile_exp c c b
    return (a' Y.:> b')
yExp _ _ = Nothing

yType :: Compiler -> Type -> Maybe Y.TypY
yType _ (ConT "Integer") = Just $ Y.VarT "int"
yType _ _ = Nothing

integerY :: Compiler
integerY = Compiler yExp yType

