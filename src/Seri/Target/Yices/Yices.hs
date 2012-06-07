
module Seri.Target.Yices.Yices (yicesY) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Target.Yices.Builtins.Prelude
import Seri.Utils.Ppr

-- Translate a seri expression to a yices expression
yExp :: Compiler -> Exp -> Maybe ([Y.CmdY], Y.ExpY)
yExp _ (IntegerE x) = Just $ ([], Y.LitI x)
yExp _ _ = Nothing

yType :: Compiler -> Type -> Maybe Y.TypY
yType _ _ = Nothing

coreY :: Compiler
coreY = Compiler yExp yType

yicesY :: Compiler
yicesY = compilers [preludeY, coreY]
            