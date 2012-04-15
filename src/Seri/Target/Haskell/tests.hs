
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import Seri
import Seri.Lib.Prelude
import Seri.Target.Haskell.Haskell

[s|
    foo :: Integer
    foo = (\x -> x*x+3*x+2) 5
|]

emain :: Env Exp
emain = typed [s| foo |]

hsMain :: Doc -> Doc
hsMain me = 
    text "main :: Prelude.IO ()" $+$
    text "main = Prelude.putStrLn" <+> (parens $ text "Prelude.show " <+> (parens me))

builtin = arithB

main :: IO ()
main = do
    args <- getArgs
    let outfile = head args
    let doc = haskell builtin hsMain emain
    writeFile outfile (show doc)

