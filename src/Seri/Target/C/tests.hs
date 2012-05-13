
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import Seri
import Seri.Lib.Prelude
import Seri.Target.C.C

[s|
    allpassed :: Bool
    allpassed = True

    returncode :: Integer
    returncode = 
        if allpassed
            then 0
            else 1
|]

declcommit

emain :: Env Exp
emain = minimize $ typed [s| returncode |]

cMain :: Doc -> Doc
cMain me = 
    text "int main(void)" $+$ (braces $
        text "return " <+> me <+> text ";")

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

