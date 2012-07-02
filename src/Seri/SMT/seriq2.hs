
import System.Environment
import System.Exit

import Seri.Failable
import Seri.Lambda
import Seri.Target.Elaborate
import Seri.SMT.Yices2

queryR = elaborateR

main :: IO ()
main = do
    args <- getArgs
    let (dbg, path, fin) =
            case args of
               ["-d", dbgout, "-i", path, fin] -> (Just dbgout, path, fin)
               ["-i", path, fin] -> (Nothing, path, fin)
               x -> error $ "bad args: " ++ show x

    query <- load [path] fin
    decs <- attemptIO $ typeinfer (flatten query)
    attemptIO $ typecheck decs

    let opts = (RunOptions dbg 30)
    tmain <- attemptIO $ lookupVarType decs "main"
    result <- runYices queryR opts decs (VarE (Sig "main" tmain))
    putStrLn $ pretty result

