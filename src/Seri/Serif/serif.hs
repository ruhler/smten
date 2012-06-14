
import System.Environment

import Seri.Lambda(parse)
import Seri.Serif.Serif

import Language.Haskell.TH


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
    seri <- parse text
    let hs = serif seri
    output $ unlines [
        "{-# LANGUAGE ExplicitForAll #-}",
        "{-# LANGUAGE MultiParamTypeClasses #-}",
        "import Prelude(Char, Integer, Bool, undefined)",
        "import qualified Prelude",
        "import Seri.Lambda.IR",
        "import Seri.Lambda.Ppr",
        "import Seri.Serif.Typed",
        show (ppr hs),
        "main :: Prelude.IO ()",
        "main = Prelude.putStrLn (pretty declarations)"
        ]

