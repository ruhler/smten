
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Seri
import Seri.SMT.SMT
import Seri.Lib.Prelude

[s|
    main :: Query (Answer Integer)
    main = do
        x <- free
        assert (x < 6)
        assert (x > 4)
        query x
|]

declcommit

theenv :: [Dec]
theenv =
 let m = [s| main |]
     e = minimize (typed m)
 in decls e

main :: IO ()
main = putStrLn (render (ppr theenv))

