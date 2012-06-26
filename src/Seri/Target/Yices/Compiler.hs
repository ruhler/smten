
module Seri.Target.Yices.Compiler (
    YCM(), runYCM, fromYCM, Compiler(..), compilers, yicesname) where

import Data.Maybe

import Seri.Lambda
import qualified Math.SMT.Yices.Syntax as Y

-- Yices compiling monad.
newtype YCM a = YCM (Either String a)

instance Monad YCM where
    -- fail: indicate the compiler does not support this expression.
    fail msg = YCM (Left msg)
    return = YCM . return
    (>>=) (YCM mx) f = YCM $ do
        x <- mx
        let (YCM fx) = f x
        fx

data Compiler = Compiler {
    compile_exp :: Compiler -> Exp -> YCM Y.ExpY,
    compile_type :: Compiler -> Type -> YCM Y.TypY,
    compile_dec :: Compiler -> Dec -> YCM [Y.CmdY]
}

compilers :: [Compiler] -> Compiler
compilers [c] = c
compilers (r:rs) = 
    let ye c e = case compile_exp r c e of
                    YCM (Right e') -> YCM (Right e')
                    YCM (Left _) -> compile_exp (compilers rs) c e

        yt c t = case compile_type r c t of
                    YCM (Right t') -> YCM (Right t')
                    YCM (Left _) -> compile_type (compilers rs) c t

        yd c d = case compile_dec r c d of
                    YCM (Right d') -> YCM (Right d')
                    YCM (Left _) -> compile_dec (compilers rs) c d
    in Compiler ye yt yd
            

runYCM :: (Monad m) => YCM a -> m a
runYCM (YCM (Right v)) = return v
runYCM (YCM (Left msg)) = fail msg

fromYCM :: YCM a -> a
fromYCM (YCM (Right v)) = v
fromYCM (YCM (Left msg)) = error msg

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

