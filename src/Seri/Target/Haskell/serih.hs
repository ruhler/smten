
-- | Compile a seri program to haskell.
module Main where

import System.Environment

import Seri.Failable
import Seri.Lambda
import Seri.Target.Haskell
import Seri.Target.Haskell.Builtins.Prelude

main :: IO ()
main = do
    args <- getArgs
    let (output, path, me, input) =
            case args of
               ["-o", fout, "-i", path, "-m", me, fin] ->
                    (writeFile fout, path, me, fin)
               x -> error $ "bad args: " ++ show x

    seri <- load [path] input
    decs <- attemptIO $ typeinfer (flatten seri)
    attemptIO $ typecheck decs
    let haskelled = haskell haskellH decs me
    output (show haskelled)

