
module Seri.Target.Yices.Builtins.Integer (
    integerY
  ) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler


yExp :: Compiler -> Exp -> YCM Y.ExpY
yExp _ _ = fail "integerY doesn't apply"

yType :: Compiler -> Type -> YCM Y.TypY
yType _ (ConT "Integer") = return (Y.VarT "int")
yType _ _ = fail "integerY doesn't apply"

-- defbinop name type op
--   Define a primitive binary operation
--   name - the name of the primitive
--   type - the yices type of the primitive
--   body - the body of the operation, referince args "a" and "b".
defbinop :: String -> String -> String -> Y.CmdY
defbinop name ty body =
    Y.DEFINE (yicesname name, Y.VarT ty)
        (Just (Y.VarE $ "(lambda (a::int) (lambda (b::int) " ++ body ++ "))"))

yDec :: Compiler -> Dec -> YCM [Y.CmdY]
yDec _ (PrimD (TopSig "__prim_add_Integer" _ _))
 = return [defbinop "__prim_add_Integer" "(-> int (-> int int))" "(+ a b)"]
yDec _ (PrimD (TopSig "__prim_sub_Integer" _ _))
 = return [defbinop "__prim_sub_Integer" "(-> int (-> int int))" "(- a b)"]
yDec _ (PrimD (TopSig "<" _ _))
 = return [defbinop "<" "(-> int (-> int Bool))"
        "(if (< a b) True False) "]
yDec _ (PrimD (TopSig ">" _ _))
 = return [defbinop ">" "(-> int (-> int Bool))"
        "(if (> a b) True False) "]
yDec _ (PrimD (TopSig "__prim_eq_Integer" _ _))
 = return [defbinop "__prim_eq_Integer" "(-> int (-> int Bool))"
        "(if (= a b) True False) "]
yDec _ _ = fail "integerY doesn't apply"

integerY :: Compiler
integerY = Compiler yExp yType yDec

