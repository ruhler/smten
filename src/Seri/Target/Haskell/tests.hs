
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

builtin = Builtin {
    mapprim = \s -> Just $ "Prim." ++ s,
    maptype = \s -> Nothing,
    includes = text "import Prelude(Integer, IO, putStrLn, (.), show, ($))" $+$
               text "import qualified Seri.Target.Haskell.Lib.Primitives as Prim"
}

main :: IO ()
main = do
    args <- getArgs
    let outfile = head args
    let doc = haskell builtin hsMain emain
    writeFile outfile (show doc)

