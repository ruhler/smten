
module Seri.Target.Yices.Builtins.Integer (
    integerY
  ) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Failable
import Seri.Lambda
import Seri.Target.Yices.Compiler


yExp :: YCompiler -> Exp -> Failable Y.ExpY
yExp _ _ = fail "integerY doesn't apply"

yType :: YCompiler -> Type -> Failable Y.TypY
yType _ _ = fail "integerY doesn't apply"

-- defiop name type op
--   Define a primitive binary integer operation.
--   name - the name of the primitive
--   op - the integer operation.
defiop :: String -> String -> Y.CmdY
defiop name op =
    Y.DEFINE (yicesname name, Y.VarT "(-> Integer (-> Integer Integer))")
        (Just (Y.VarE $
            "(lambda (a::Integer) (lambda (b::Integer) (" ++ op ++ " a b)))"))

-- defbop name type op
--   Define a primitive binary integer predicate.
--   name - the name of the primitive
--   op - the predicate operator.
defbop :: String -> String -> Y.CmdY
defbop name op =
    Y.DEFINE (yicesname name, Y.VarT "(-> Integer (-> Integer Bool))")
        (Just (Y.VarE $ unlines [
                "(lambda (a::Integer) (lambda (b::Integer)",
                " (if (" ++ op ++ " a b) True False)))"]))

yDec :: YCompiler -> Dec -> Failable [Y.CmdY]
yDec _ (PrimD (TopSig "__prim_add_Integer" _ _))
 = return [defiop "__prim_add_Integer" "+"]
yDec _ (PrimD (TopSig "__prim_sub_Integer" _ _))
 = return [defbop "__prim_sub_Integer" "-"]
yDec _ (PrimD (TopSig "<" _ _)) = return [defbop "<" "<"]
yDec _ (PrimD (TopSig ">" _ _)) = return [defbop ">" ">"]
yDec _ (PrimD (TopSig "__prim_eq_Integer" _ _))
 = return [defbop "__prim_eq_Integer" "="]
yDec _ _ = fail "integerY doesn't apply"

integerY :: YCompiler
integerY = Compiler yExp yType yDec

