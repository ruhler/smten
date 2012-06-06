
import Seri
import Seri.Lib.Prelude
import Seri.SMT.Yices

queryR = rules [yicesR, preludeR]

main :: IO ()
main = do
    querytext <- getContents
    query <- parseDecs querytext
    result <- runYices queryR (mkenv query (VarE (Sig "main" (AppT (ConT "Query") (ConT "?"))) Declared))
    putStrLn $ render (ppr result)

