
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import Seri
import Seri.Lib.Prelude
import Seri.Target.Haskell.Haskell

[s|
    foo :: Integer
    foo = (\x -> x*x+3*x+2) 5

    allpassed :: Bool
    allpassed = foo == 42
|]

emain :: Env Exp
emain = typed [s| allpassed |]

hsMain :: Doc -> Doc
hsMain me = 
    text "main :: Prelude.IO ()" $+$
    text "main = if " <+> me <+>
        text " then System.Exit.exitSuccess else System.Exit.exitFailure"

builtin = builtins [preludeB, Builtin {
    mapprim = \_ -> Nothing,
    maptype = \_ -> Nothing,
    includes = text "import qualified System.Exit"
}]

main :: IO ()
main = do
    args <- getArgs
    let outfile = head args
    let doc = haskell builtin hsMain emain
    writeFile outfile (show doc)

