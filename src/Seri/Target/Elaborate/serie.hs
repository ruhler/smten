
import System.Environment

import Seri.Lambda
import Seri.Target.Elaborate

main :: IO ()
main = do
    args <- getArgs
    let (output, path, mainexp, input) =
            case args of
               ["-o", fout, "-i", path, "-m", me, fin] ->
                    (writeFile fout, path, me, fin)
               ["-i", path, fin] -> (putStrLn, path, "main", fin)
               x -> error $ "bad args: " ++ show x

    seri <- load [path] input
    let e = mkenv (flatten seri) (VarE (Sig mainexp UnknownT) Declared)
    elaborated <- elaborate elaborateR e
    output (pretty elaborated)

