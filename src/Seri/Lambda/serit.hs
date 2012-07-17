
-- | Run type inference and type checking of the given seri program and print
-- out the resulting typed expression.
module Main where

import System.Environment

import Seri.Failable
import Seri.Lambda

main :: IO ()
main = do
    args <- getArgs
    let (output, path, input) =
            case args of
               ["-o", fout, "-i", path, fin] ->
                    (writeFile fout, path, fin)
               ["-i", path, fin] -> (putStrLn, path, fin)
               x -> error $ "bad args: " ++ show x

    seri <- load [path] input
    let decs = flatten seri
    decs' <- attemptIO $ typeinfer decs
    attemptIO $ typecheck decs'
    output $ pretty decs'

