
module Seri.Target.Yices.Compiler (Compiler(..), compilers) where

import Seri.Lambda
import qualified Math.SMT.Yices.Syntax as Y

data Compiler = Compiler {
    compile_exp :: Compiler -> Exp -> Maybe Y.ExpY,
    compile_type :: Compiler -> Type -> Maybe Y.TypY
}

compilers :: [Compiler] -> Compiler
compilers [] = Compiler (\_ _ -> Nothing) (\_ _ -> Nothing) 
compilers (r:rs) = 
    let ye c e = case compile_exp r c e of
                    Just e' -> Just e'
                    Nothing -> compile_exp (compilers rs) c e

        yt c t = case compile_type r c t of
                    Just t' -> Just t'
                    Nothing -> compile_type (compilers rs) c t
    in Compiler ye yt
            
