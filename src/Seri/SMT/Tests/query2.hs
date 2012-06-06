
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Seri
import Seri.SMT.SMT
import Seri.Lib.Prelude

[s|
    incr :: Integer -> Integer
    incr a = a + 1 

    main :: Query (Answer Integer)
    main = do
        x <- free
        assert (x < 6)
        assert ((incr x) > 5)
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

