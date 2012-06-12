
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Seri
import Seri.SMT.SMT
import Seri.Lib.Prelude

[s|
    data Foo = Bar
             | Sludge Bool

    defoo :: Foo -> Integer
    defoo f = case f of
                (Sludge True) -> 0
                (Sludge False) -> 1

    isBar :: Foo -> Bool
    isBar Bar = True
    isBar (Sludge _) = False

    main :: Query (Answer Foo)
    main = do
        f <- free
        assert (isBar f)
        assert (1 == defoo f)
        query f
|]

declcommit

theenv :: [Dec]
theenv =
 let m = [s| main |]
     e = typed m
 in decls e

main :: IO ()
main = putStrLn (render (ppr theenv))

