
import System.Environment

import Seri.Lambda
import Seri.Utils.Ppr
import Seri.Serif.Serif


main :: IO ()
main = do
    args <- getArgs
    let (output, input) =
            case args of
               ["-o", fout, fin] -> (writeFile fout, readFile fin)
               ["-o", fout] -> (writeFile fout, getContents)
               [fin] -> (putStrLn, readFile fin)
               [] -> (putStrLn, getContents)
               x -> error $ "bad args: " ++ show x

    text <- input
    seri <- parseDecs text
    let hs = serif seri
    output $ unlines [
        "import Seri.Lambda.IR",
        "import Seri.FrontEnd.Typed",
        "import Seri.Serif.Library",
        render (ppr hs),
        "main :: IO ()",
        "main = putStrLn \"hello!\""
        ]

