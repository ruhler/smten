
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Seri
import Seri.SMT.SMT
import Seri.Lib.Prelude

[s|
    data Foo = Bar Integer
             | Sludge Bool

    defoo :: Foo -> Integer
    defoo (Bar x) = x
    defoo (Sludge b) = if b then 1 else 0

    foo :: Integer -> Integer
    foo x = x + 1

    scopedfail :: Query Bool
    scopedfail = do
        x <- free
        assert(x < x)
        q <- query x
        case q of
            Satisfiable _ -> return True
            _ -> return False

    main :: Query (Answer (Bool, Integer, Bool, Foo))
    main = do
        b <- free
        assert b

        x <- free
        assert ((if x < 0 then x else foo x) == 4)

        sf <- scoped scopedfail

        f <- free
        assert (2 == defoo f)
        query (b, x, sf, f)
|]

declcommit

theenv :: [Dec]
theenv =
 let m = [s| main |]
     e = minimize (typed m)
 in decls e

main :: IO ()
main = putStrLn (render (ppr theenv))

