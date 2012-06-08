
import Seri
import Seri.Lib.Prelude
import Seri.SMT.Yices

queryR = rules [yicesR, preludeR]

main :: IO ()
main = do
    querytext <- getContents
    query <- parseDecs querytext
    let e = (mkenv query (VarE (Sig "main" (AppT (ConT "Query") (ConT "?"))) Declared))
    let opts = (RunOptions (Just "debug.out") "/home/ruhler/local/bin/yices")
    result <- runYices queryR opts e
    putStrLn $ render (ppr result)

