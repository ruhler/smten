

module Seri.Target.C.C (
    c
    ) where

import Data.Maybe(fromJust)

import Seri
import qualified Seri.Target.C.AST as C
import Seri.Target.C.Builtin

-- c builtin main exp
--  Compile the given expression and its environment to c.
--
--  builtin - a description of builtin things.
--  main - A function to generate the main function in the c code.
--         The input to the function is the c text for the compiled
--         expression.
--  exp - The expression to compile to c.
c :: Builtin -> (Doc -> Doc) -> Env Exp -> Doc
c builtin main e =
  let
      -- Given a seri type constructor, return the name of the c structure
      -- for that type.
      tcname :: Name -> C.Name
      tcname nm = "tc_" ++ nm

      -- Given a type of the form a -> b -> ... -> r
      --   Return (r, [a, b, ...])
      flattentapp :: Type -> (Type, [Type])
      flattentapp (AppT a b)
        = let (r, targs) = flattentapp b
          in (r, a:targs)
      flattentapp t = (t, [])

      cExp :: Exp -> C.Exp
      cExp e | mapexp builtin e /= Nothing = fromJust (mapexp builtin e)
      cExp (IntegerE i) = C.IntE i
      cExp (IfE _ p a b) = C.CondE (cExp p) (cExp a) (cExp b)
      cExp (ConE _ nm) = C.AppE nm []
      cExp (VarE _ nm Declared) = C.AppE nm []
      cExp x = error $ "TODO: cExp " ++ show x

      cType :: Type -> C.Type
      cType t | maptype builtin t /= Nothing = fromJust (maptype builtin t)
      cType (ConT nm) = C.StructT (tcname nm)
      cType x = error $ "TODO: cType " ++ show x

      cDec :: Dec -> C.Dec
      cDec (ValD nm t v)
        = let (r, []) = flattentapp t
          in C.FunD (cType r) nm [] (C.ReturnS (cExp v))
      cDec x = error $ "TODO: cDec " ++ show x

      ds = map cDec (decls e)
      me = cExp $ val e
  in includes builtin $+$
     ppr ds $+$
     main (ppr me)
     

