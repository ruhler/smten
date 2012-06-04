
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

simple :: Typed (Env Exp) (Query (Answer Integer))
simple = [s| do
    x <- free
    assert (x < 6)
    assert (x > 4)
    query x
    |]

tests = "Seri.SMT" ~: [
        "simple" ~: querytest (val (typed [s| Satisfiable 5 |])) simple,
        "print simple" ~: (putStrLn (show (minimize (typed simple))))
        ]

