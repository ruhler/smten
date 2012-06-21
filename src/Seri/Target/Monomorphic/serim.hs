
import System.Environment

import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic

main :: IO ()
main = do
    args <- getArgs
    let ["-i", path, fin] = args

    poly <- load [path] fin
    let e = mkenv (flatten poly) (VarE (Sig "main" UnknownT))
    let mono = monomorphic e
    putStrLn $ pretty mono

