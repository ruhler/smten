
module Seri.Target.Yices.Builtins.Integer (
    integerY
  ) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Utils.Ppr
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

yExp :: Compiler -> Exp -> Maybe ([Y.CmdY], Y.ExpY)
yExp c (PrimE (Sig "+" _)) = return ([], Y.VarE "__prim_add")
yExp c (PrimE (Sig "-" _)) = return ([], Y.VarE "__prim_sub")
yExp c (PrimE (Sig "<" _)) = return ([], Y.VarE "__prim_lt")
yExp c (PrimE (Sig ">" _)) = return ([], Y.VarE "__prim_gt")
yExp c (PrimE (Sig "==" _)) = return ([], Y.VarE "__prim_eq")
yExp _ _ = Nothing

yType :: Compiler -> Type -> Maybe Y.TypY
yType _ (ConT "Integer") = Just $ Y.VarT "int"
yType _ _ = Nothing

integerY :: Compiler
integerY = Compiler yIncludes yExp yType

