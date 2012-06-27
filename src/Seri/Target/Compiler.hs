
module Seri.Target.Compiler (
    Compiler(..), compilers,
    compile_decs
    ) where

import Data.Maybe

import Seri.Failable
import Seri.Lambda

data Compiler e t d = Compiler {
    compile_exp :: Compiler e t d -> Exp -> Failable e,
    compile_type :: Compiler e t d -> Type -> Failable t,
    compile_dec :: Compiler e t d -> Dec -> Failable [d]
}

compilers :: [Compiler e t d] -> Compiler e t d
compilers [c] = c
compilers (r:rs) = 
    let ye c e = compile_exp r c e <|> compile_exp (compilers rs) c e
        yt c t = compile_type r c t <|> compile_type (compilers rs) c t
        yd c d = compile_dec r c d <|> compile_dec (compilers rs) c d
    in Compiler ye yt yd

compile_decs :: Compiler e t d -> [Dec] -> [d]
compile_decs c ds = surely $ do
    ds' <- mapM (compile_dec c c) ds
    return $ concat ds'

