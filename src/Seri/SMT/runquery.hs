
import System.Environment
import System.Exit
import System.Process

import Seri.Failable
import Seri.Lambda
import Seri.Target.Elaborate
import Seri.SMT.Yices

queryR = elaborateR

main :: IO ()
main = do
    args <- getArgs
    let (dbg, path, fin) =
            case args of
               ["-d", dbgout, "-i", path, fin] -> (Just dbgout, path, fin)
               ["-i", path, fin] -> (Nothing, path, fin)
               x -> error $ "bad args: " ++ show x

    (ex, out, err) <- readProcessWithExitCode "which" ["yices"] ""
    yices <- case (ex, out) of
                (ExitSuccess, y) -> return $ head (words y)
                _ -> fail $ "Failed to find yices executable: " ++ err


    query <- load [path] fin
    decs <- attemptIO $ typeinfer (flatten query)
    attemptIO $ typecheck decs

    let e = mkenv decs (VarE (Sig "main" UnknownT))

    let opts = (RunOptions dbg yices)
    result <- runYices [] queryR opts e
    putStrLn $ pretty result

