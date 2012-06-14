
import System.Environment

import Seri.Lambda
import Seri.Target.Elaborate

main :: IO ()
main = do
    args <- getArgs
    let (output, mainexp, input) =
            case args of
               ["-o", fout, "-m", me, fin] -> (writeFile fout, me, readFile fin)
               ["-o", fout] -> (writeFile fout, "main", getContents)
               [fin] -> (putStrLn, "main", readFile fin)
               [] -> (putStrLn, "main", getContents)
               x -> error $ "bad args: " ++ show x

    text <- input
    seri <- parseDecs text
    let e = mkenv seri (VarE (Sig mainexp UnknownT) Declared)
    elaborated <- elaborate elaborateR e
    output (pretty elaborated)

