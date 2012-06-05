
{-# LANGUAGE QuasiQuotes #-}

import Seri
import Seri.Lib.Prelude

library :: [Dec]
library = decls (typed [s| 0 |])

mystyle :: Style
mystyle = style { lineLength = 80 }

main :: IO ()
main = putStrLn (renderStyle mystyle (ppr library))
    
