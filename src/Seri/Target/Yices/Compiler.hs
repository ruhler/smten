
module Seri.Target.Yices.Compiler (
    YCM(), runYCM, fromYCM, Compiler(..), compilers) where

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
    includes :: [Y.CmdY],
    compile_exp :: Compiler -> Exp -> YCM Y.ExpY,
    compile_type :: Compiler -> Type -> YCM Y.TypY
}

compilers :: [Compiler] -> Compiler
compilers [c] = c
compilers (r:rs) = 
    let yi = includes r ++ includes (compilers rs)
        ye c e = case compile_exp r c e of
                    YCM (Right e') -> YCM (Right e')
                    YCM (Left _) -> compile_exp (compilers rs) c e

        yt c t = case compile_type r c t of
                    YCM (Right t') -> YCM (Right t')
                    YCM (Left _) -> compile_type (compilers rs) c t
    in Compiler yi ye yt
            

runYCM :: (Monad m) => YCM a -> m a
runYCM (YCM (Right v)) = return v
runYCM (YCM (Left msg)) = fail msg

fromYCM :: YCM a -> a
fromYCM (YCM (Right v)) = v
fromYCM (YCM (Left msg)) = error msg

