
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Seri.Tests (
    tests
    ) where

import Seri
import Seri.Lib.Prelude

import Test.HUnit


seriR :: Rule
seriR = rules [coreR, arithR]

eval :: TEnv Exp a -> Exp
eval e = elaborate seriR (typed e)

eqexp :: Exp -> TEnv Exp a -> Assertion
eqexp wnt e = do
    let got = eval e
    if (wnt /= got)
        then assertFailure $ unlines [
            "expected:" ++ (show . ppr $ wnt),
            " but got:" ++ (show . ppr $ got),
            "with env: " ++ (show . ppr . env . typed $ e)
            ]
        else return ()

[s|
    foo :: Integer
    foo = (\x -> x*x+3*x+2) 5

    rfact :: Integer -> Integer
    rfact x = if (x < 1) then 1 else x * rfact (x-1)

    id :: a -> a
    id x = x
|]


[s|
    data MaybeInteger = NoInteger | JustInteger Integer

    fromMaybeInteger :: Integer -> MaybeInteger -> Integer
    fromMaybeInteger def = \mi ->
        case mi of
            JustInteger i -> i
            NoInteger -> def
|]

decltype ''Maybe
[s|
    fromMaybeBool :: Bool -> Maybe Bool -> Bool
    fromMaybeBool def = \mb ->
        case mb of
            Just b -> b
            Nothing -> def
|]

[s|
    multclause :: Integer -> Integer
    multclause 2 = 10
    multclause 3 = 20
    multclause 4 = 30   
    multclause 5 = 40
    multclause _ = 50
|]

[s|
    tupleswap :: (a, b) -> (b, a)
    tupleswap (x, y) = (y, x)
|]

[s|
    listswaptop :: [a] -> [a]
    listswaptop (x:xs) = (head xs) : x : (tail xs)

    listdifftop :: [Integer] -> Integer
    listdifftop (x:y:_) = x - y
|]

[s|
    sum2 :: Integer -> Integer -> Integer
    sum2 a b = a + b

    sum3 :: Integer -> Integer -> Integer -> Integer
    sum3 a b c = a + b + c
|]

[s|
    unary2int :: [()] -> Integer
    unary2int [] = 0
    unary2int (_:xs) = 1 + unary2int xs
|]


tests = "Seri" ~: [
    "foo" ~: IntegerE 42 `eqexp` [s|(\x -> x*x+3*x+2) 5|],
    "true" ~: trueE `eqexp` [s| True |],
    "if" ~: IntegerE 23 `eqexp` [s| if 6 < 4 then 42 else 23 |],
    "slice" ~: IntegerE 7 `eqexp` [s| 3 + _s (integerE . toInteger $ length [4,1,5,56]) |],
    "foo decl" ~: IntegerE 42 `eqexp` _seriC_foo,
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
    "unit type" ~: IntegerE 3 `eqexp` [s| unary2int [(), (), ()] |]
    ]

