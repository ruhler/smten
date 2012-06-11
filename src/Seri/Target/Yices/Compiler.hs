
module Seri.Target.Yices.Compiler (
    YCM(), runYCM, Compiler(..), compilers) where

import Seri.Lambda
import qualified Math.SMT.Yices.Syntax as Y

-- Yices compiling monad.
newtype YCM a = YCM (Maybe a)

instance Monad YCM where
    -- fail: indicate the compiler does not support this expression.
    fail msg = YCM Nothing
    return = YCM . return
    (>>=) (YCM mx) f = YCM $ do
        x <- mx
        let (YCM fx) = f x
        fx

-- TODO: probably want compile_type to be in the YCM monad too, instead of
-- Maybe. (I've just been too lazy to move it there thus far)
data Compiler = Compiler {
    includes :: [Y.CmdY],
    compile_exp :: Compiler -> Exp -> YCM Y.ExpY,
    compile_type :: Compiler -> Type -> Maybe Y.TypY
}

compilers :: [Compiler] -> Compiler
compilers [] = Compiler [] (\_ _ -> YCM Nothing) (\_ _ -> Nothing) 
compilers (r:rs) = 
    let yi = includes r ++ includes (compilers rs)
        ye c e = case compile_exp r c e of
                    YCM (Just e') -> YCM (Just e')
                    YCM Nothing -> compile_exp (compilers rs) c e

        yt c t = case compile_type r c t of
                    Just t' -> Just t'
                    Nothing -> compile_type (compilers rs) c t
    in Compiler yi ye yt
            

runYCM :: YCM a -> Maybe a
runYCM (YCM x) = x

