
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Seri
import Seri.SMT.SMT
import Seri.Lib.Prelude

[s|
    sign :: Integer -> Integer
    sign a = if a < 0 then 0 else 1

    main :: Query (Answer Integer)
    main = do
        x <- free
        assert ((sign x) == 0)
        query x
|]

declcommit

theenv :: [Dec]
theenv =
 let m = [s| main |]
     e = typed m
 in decls e

main :: IO ()
main = putStrLn (render (ppr theenv))

