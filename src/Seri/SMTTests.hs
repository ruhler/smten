
{-# LANGUAGE QuasiQuotes #-}

module Seri.SMTTests (tests) where

import Test.HUnit

import Seri
import Seri.SMT

-- querytest expected decls query 
querytest :: Exp -> [Dec] -> Typed Exp (Query a) -> Assertion
querytest exp decls q = do
    --r <- runQuery (rules [coreR, arithR]) decls q
    --assertEqual "" exp (typed r)
    return ()


tests = "Seri.SMT" ~: [
        "simple" ~: querytest (IntegerE 5) [] [s|
            do  x <- free
                assert (x < 6)
                assert (x > 4)
                qr <- query x
                case qr of
                    Satisfiable v -> return v
                    _ -> return 0
            |]
        ]

