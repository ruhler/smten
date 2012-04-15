
{-# LANGUAGE QuasiQuotes #-}

module Seri.SMT.Tests (tests) where

import Test.HUnit

import Seri
import Seri.Lib.Prelude
import Seri.SMT.SMT

-- querytest expected decls query 
querytest :: Exp -> Typed (Env Exp) (Query a) -> Assertion
querytest exp q = do
    --r <- runQuery (rules [coreR, arithR]) decls q
    --assertEqual "" exp (typed r)
    return ()


tests = "Seri.SMT" ~: [
        "simple" ~: querytest (IntegerE 5) [s|
            do  x <- free
                assert (x < 6)
                assert (x > 4)
                qr <- query x
                case qr of
                    Satisfiable v -> return v
                    _ -> return 0
            |]
        ]

