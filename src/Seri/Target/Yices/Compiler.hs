
module Seri.Target.Yices.Compiler (Compiler(..), compilers, yicesname) where

import Data.Maybe

import Seri.Failable
import Seri.Lambda
import qualified Math.SMT.Yices.Syntax as Y

data Compiler = Compiler {
    compile_exp :: Compiler -> Exp -> Failable Y.ExpY,
    compile_type :: Compiler -> Type -> Failable Y.TypY,
    compile_dec :: Compiler -> Dec -> Failable [Y.CmdY]
}

compilers :: [Compiler] -> Compiler
compilers [c] = c
compilers (r:rs) = 
    let ye c e = compile_exp r c e <|> compile_exp (compilers rs) c e
        yt c t = compile_type r c t <|> compile_type (compilers rs) c t
        yd c d = compile_dec r c d <|> compile_dec (compilers rs) c d
    in Compiler ye yt yd
            
-- Given a seri identifer, turn it into a valid yices identifier.
-- TODO: hopefully our choice of names won't clash with the users choices...
--
-- I don't have documentation for what yices allows in names, but it appears
-- symbols aren't allowed. So this just replaces each symbol with an ascii
-- approximation.
yicesname :: String -> String
yicesname [] = []
-- TODO: renaming of 'not' should be part of builtins, it should not go here.
yicesname "not" = "_not"
yicesname ('!':cs) = "__bang" ++ yicesname cs
yicesname ('#':cs) = "__hash" ++ yicesname cs
yicesname ('$':cs) = "__dollar" ++ yicesname cs
yicesname ('%':cs) = "__percent" ++ yicesname cs
yicesname ('&':cs) = "__amp" ++ yicesname cs
yicesname ('*':cs) = "__star" ++ yicesname cs
yicesname ('+':cs) = "__plus" ++ yicesname cs
yicesname ('.':cs) = "__dot" ++ yicesname cs
yicesname ('/':cs) = "__slash" ++ yicesname cs
yicesname ('<':cs) = "__lt" ++ yicesname cs
yicesname ('=':cs) = "__eq" ++ yicesname cs
yicesname ('>':cs) = "__gt" ++ yicesname cs
yicesname ('?':cs) = "__ques" ++ yicesname cs
yicesname ('@':cs) = "__at" ++ yicesname cs
yicesname ('\\':cs) = "__bslash" ++ yicesname cs
yicesname ('^':cs) = "__hat" ++ yicesname cs
yicesname ('|':cs) = "__bar" ++ yicesname cs
yicesname ('-':cs) = "__dash" ++ yicesname cs
yicesname ('~':cs) = "__tilde" ++ yicesname cs
yicesname ('(':cs) = "__oparen" ++ yicesname cs
yicesname (')':cs) = "__cparen" ++ yicesname cs
yicesname (',':cs) = "__comma" ++ yicesname cs
yicesname (c:cs) = c : yicesname cs

