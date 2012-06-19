
import System.Environment
import System.Exit

import Seri.Failable
import Seri.Lambda
import Seri.Serif

import Language.Haskell.TH

main :: IO ()
main = do
    args <- getArgs
    let (fout, fin) =
            case args of
               ["-o", fout, fin] -> (fout, fin)
               x -> error $ "bad args: " ++ show x

    text <- readFile fin
    seri <- attemptIO $ parse fin text
    let hs = serif seri
    writeFile fout $ unlines [
        "{-# LANGUAGE ExplicitForAll #-}",
        "{-# LANGUAGE MultiParamTypeClasses #-}",
        show (ppr hs)
        ]

