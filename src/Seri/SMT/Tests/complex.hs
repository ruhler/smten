
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Seri
import Seri.SMT.SMT
import Seri.Lib.Prelude

[s|
    main :: Query (Answer Bool)
    main = do
        b <- free
        assert b
        query b
|]

declcommit

theenv :: [Dec]
theenv =
 let m = [s| main |]
     e = minimize (typed m)
 in decls e

main :: IO ()
main = putStrLn (render (ppr theenv))

