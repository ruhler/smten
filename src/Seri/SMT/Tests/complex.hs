
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Seri
import Seri.SMT.SMT
import Seri.Lib.Prelude

[s|
    foo :: Integer -> Integer
    foo x = x + 1

    main :: Query (Answer (Bool, Integer))
    main = do
        b <- free
        assert b

        x <- free
        assert ((if x < 0 then x else foo x) == 4)
        query (b, x)
|]

declcommit

theenv :: [Dec]
theenv =
 let m = [s| main |]
     e = minimize (typed m)
 in decls e

main :: IO ()
main = putStrLn (render (ppr theenv))

