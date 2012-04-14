
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
    text "main :: IO ()" $+$
    text "main = putStrLn . show $" <+> me

main :: IO ()
main = do
    args <- getArgs
    let outfile = head args
    let doc = text "import Prelude(Integer, IO, putStrLn, (.), show, ($))"
              $+$ haskell hsMain emain
    writeFile outfile (show doc)

