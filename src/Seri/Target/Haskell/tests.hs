
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import Seri
import Seri.Lib.Prelude
import Seri.Target.Haskell.Haskell

[s|
    foo :: Integer -> Integer
    foo x = x*x+3*x+2

    unary2int :: [()] -> Integer
    unary2int [] = 0
    unary2int (_:xs) = 1 + unary2int xs

    allpassed :: Bool
    allpassed =
           foo 5 == 42
        && unary2int [(), (), ()] == 3
|]

declcommit

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

