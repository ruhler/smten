
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import Seri
import Seri.Lib.Prelude
import Seri.Target.C.C

[s|
    allpassed :: Bool
    allpassed = True
|]

declcommit

emain :: Env Exp
emain = typed [s| allpassed |]

cMain :: Doc -> Doc
cMain me = 
    text "int main(void)" $+$ (braces $
        text "return " <+> me <+> text " ? 0 : 1;")

builtin = builtins [Builtin {
    mapprim = \_ -> Nothing,
    maptype = \_ -> Nothing,
    includes = empty
}]

main :: IO ()
main = do
    args <- getArgs
    let outfile = head args
    let doc = c builtin cMain emain
    writeFile outfile (show doc)

