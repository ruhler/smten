
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Seri.Tests (
    tests
    ) where

import Seri
import Seri.Lib.Prelude
import Seri.Lib.Tests

import Test.HUnit

eval :: Typed (Env Exp) a -> Exp
eval e = elaborate preludeR (typed e)

eqexp :: Exp -> Typed (Env Exp) a -> Assertion
eqexp wnt e = do
    let got = eval e
    if (wnt /= got)
        then assertFailure $ unlines [
            "expected:" ++ (show . ppr $ wnt),
            " but got:" ++ (show . ppr $ got),
            "with env: " ++ (show . ppr . decls . typed $ e)
            ]
        else return ()


tests = "Seri" ~: [
    "foo" ~: IntegerE 42 `eqexp` [s|(\x -> x*x+3*x+2) 5|],
    "foo1dec" ~: IntegerE 42 `eqexp` [s| foo1 |],
    "foo2dec" ~: IntegerE 42 `eqexp` [s| foo2 5 |],
    "true" ~: trueE `eqexp` [s| True |],
    "if" ~: IntegerE 23 `eqexp` [s| if 6 < 4 then 42 else 23 |],
    "slice" ~: IntegerE 7 `eqexp` [s| 3 + _s (integerE . toInteger $ length [4,1,5,56]) |],
    "id id 5" ~: IntegerE 5 `eqexp` [s| (id id) 5 |],
    "justInteger" ~: IntegerE 5 `eqexp` 
            [s| fromMaybeInteger 10 (JustInteger 5) |],
    "noInteger" ~: IntegerE 10 `eqexp`
            [s| fromMaybeInteger 10 NoInteger |],
    "just Bool" ~: trueE `eqexp`
            [s| fromMaybeBool False (Just True) |],
    "no Bool" ~: falseE `eqexp`
            [s| fromMaybeBool False Nothing |],
    "int pattern" ~: IntegerE 30 `eqexp` [s|
        case (1 + 3) of
            2 -> 10
            3 -> 20
            4 -> 30
            5 -> 40 
            _ -> 50
        |],
    "multclause" ~: IntegerE 30 `eqexp` [s| multclause 4 |],
    "tuples" ~: IntegerE 30 `eqexp` 
        [s| snd (tupleswap (30, 40)) |],
    "lists" ~: IntegerE 20 `eqexp` 
        [s| listdifftop (listswaptop [10, 30, 50, 0]) |],
    "2 arg func" ~: IntegerE 12 `eqexp` [s| sum2 5 7 |],
    "3 arg func" ~: IntegerE 20 `eqexp` [s| sum3 5 7 8 |],
    "unit type" ~: IntegerE 3 `eqexp` [s| unary2int [(), (), ()] |],
    "Foo class bool" ~: IntegerE 1 `eqexp` [s| foo True |],
    "Foo class int" ~: IntegerE 2 `eqexp` [s| foo 42 |],
    "foofun bool" ~: IntegerE 5 `eqexp` [s| foofun False |],
    "foofun int" ~: IntegerE 11 `eqexp` [s| foofun 5 |],
    "green apple" ~: IntegerE 11 `eqexp` [s| numseeds (Apple True 11) |],
    "wonderful" ~: IntegerE 11 `eqexp` [s| thething (Wonderful 11 32) |]
    ]

