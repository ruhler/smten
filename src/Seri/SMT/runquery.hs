
import Seri
import Seri.Lib.Prelude

queryR = preludeR

main :: IO ()
main = do
    querytext <- getContents
    query <- parseDecs querytext
    result <- elaborate queryR (mkenv query (VarE (Sig "main" (AppT (ConT "Query") (ConT "?"))) Declared))
    putStrLn $ render (ppr result)

