
import System.Environment
import System.Exit

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
    case (parse fin text) of
        Right seri -> do
            let hs = serif seri
            writeFile fout $ unlines [
                "{-# LANGUAGE ExplicitForAll #-}",
                "{-# LANGUAGE MultiParamTypeClasses #-}",
                show (ppr hs),
                "main :: Prelude.IO ()",
                "main = Prelude.putStrLn (pretty serimodule)"
                ]
        Left msg -> do  
            putStrLn msg
            exitFailure

