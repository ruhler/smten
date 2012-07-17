
-- | Monomorphize the given seri program and print out the result.
module Main where

import System.Environment

import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic

main :: IO ()
main = do
    args <- getArgs
    let ["-i", path, fin] = args

    poly <- load [path] fin
    let (decs, exp) = monomorphic (flatten poly) (VarE (Sig "main" UnknownT))
    putStrLn $ pretty exp ++ "\n\n" ++ pretty decs

