

module Seri.Target.C.C (
    c
    ) where

import qualified Seri.Target.C.AST as C
import Seri

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
  let cExp :: Exp -> C.Exp
      cExp (VarE _ nm Declared) = C.AppE nm []
      cExp x = error $ "TODO: cExp " ++ show x

      cDec :: Dec -> C.Dec
      cDec x = error $ "TODO: cDec " ++ show x

      ds = map cDec (decls e)
      me = cExp $ val e
  in includes builtin $+$
     ppr ds $+$
     main (ppr me)
     

