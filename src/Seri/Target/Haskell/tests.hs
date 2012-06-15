

import System.Environment

import Language.Haskell.TH.PprLib

import Seri.Lambda
import Seri.Target.Haskell.Haskell
import Seri.Target.Haskell.Builtin
import Seri.Target.Haskell.Builtins.Prelude

hsMain :: Doc -> Doc
hsMain me = 
    text "main :: Prelude.IO ()" $+$
    text "main = if " <+> me <+>
        text " then System.Exit.exitSuccess else System.Exit.exitFailure"

builtin = builtins [preludeB, Builtin {
    mapexp = \_ -> Nothing,
    maptype = \_ -> Nothing,
    includes = text "import qualified System.Exit"
}]

main :: IO ()
main = do
    args <- getArgs
    let ["-o", fout, fin] = args
    text <- readFile fin
    case parse fin text of
        Right seri -> do
            let emain = mkenv seri (VarE (Sig "testall" UnknownT) Declared)
            let doc = haskell builtin hsMain emain
            writeFile fout (show doc)
        Left msg -> putStrLn msg >> exitFailure

