
module Seri.Target.Yices.Builtins.Integer (
    integerY
  ) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Target.Haskell.Builtin


-- defbinop name type op
--   Define a primitive binary operation
--   name - the name of the primitive
--   type - the yices type of the primitive
--   op - the yices operator to use for the primitive.
defbinop :: String -> String -> String -> Y.CmdY
defbinop name ty op =
    Y.DEFINE (name, Y.VarT ty)
        (Just (Y.VarE $ "(lambda (a::int) (lambda (b::int) (" ++ op ++ " a b)))"))

yIncludes :: [Y.CmdY]
yIncludes = [
        defbinop "__prim_add" "(-> int (-> int int))" "+",
        defbinop "__prim_sub" "(-> int (-> int int))" "-",
        defbinop "__prim_lt" "(-> int (-> int bool))" "<",
        defbinop "__prim_gt" "(-> int (-> int bool))" ">",
        defbinop "__prim_eq" "(-> int (-> int bool))" "="
    ]

yExp :: Compiler -> Exp -> YCM Y.ExpY
yExp c (VarE (Sig "__prim_add_Integer" _)) = return $ Y.VarE "__prim_add"
yExp c (VarE (Sig "__prim_sub_Integer" _)) = return $ Y.VarE "__prim_sub"
yExp c (VarE (Sig "<" _)) = return $ Y.VarE "__prim_lt"
yExp c (VarE (Sig ">" _)) = return $ Y.VarE "__prim_gt"
yExp c (VarE (Sig "==" _)) = return $ Y.VarE "__prim_eq"
yExp _ _ = fail "integerY doesn't apply"

yType :: Compiler -> Type -> YCM Y.TypY
yType _ (ConT "Integer") = return $ Y.VarT "int"
yType _ _ = fail "integerY doesn't apply"

integerY :: Compiler
integerY = Compiler yIncludes yExp yType

