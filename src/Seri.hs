
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Seri (
    tests
    ) where

import Seri.Declarations
import Seri.Elaborate
import Seri.IR
import Seri.Quoter
import Seri.Primitives
import Seri.Typed

import Test.HUnit


-- Test Cases for Seri

run :: [Dec] -> Typed Exp a -> Exp
run decls = elaborate coreR decls . typed

[s|
    foo :: Integer
    foo = (\x -> x*x+3*x+2) 5

    factorial :: Integer -> Integer
    factorial = fix (\f -> \x -> if (x < 1) then 1 else x * f (x-1))

    fact5 :: Integer
    fact5 = factorial 5

    fact6 :: Integer
    fact6 = 6 * fact5

    rfact :: Integer -> Integer
    rfact = \x -> if (x < 1) then 1 else x * rfact (x-1)

    id :: a -> a
    id = \x -> x
|]

data MaybeInteger = NoInteger | JustInteger Integer

decltype ''MaybeInteger

[s|
    fromMaybeInteger :: Integer -> MaybeInteger -> Integer
    fromMaybeInteger = \def -> \mi ->
        case mi of
            JustInteger i -> i
            NoInteger -> def
|]

decltype ''Maybe

[s|
    fromMaybeBool :: Bool -> Maybe Bool -> Bool
    fromMaybeBool = \def -> \mb ->
        case mb of
            Just b -> b
            Nothing -> def
|]

tests = "Seri" ~: [
    "foo" ~: IntegerE 42 ~=? run [] [s|(\x -> x*x+3*x+2) 5|],
    "unit" ~: PrimE UnitT "unit" ~=? run _seriD_unit [s| unit |],
    "true" ~: trueE ~=? run _seriD_True [s| True |],
    "if" ~: IntegerE 23 ~=? run [] [s| if 6 < 4 then 42 else 23 |],
    "slice" ~: IntegerE 7 ~=? run [] [s| 3 + _s (integerE . toInteger $ length [4,1,5,56]) |],
    "fix" ~: IntegerE 120 ~=?
        let factorial = [s| fix (\f -> \x -> if (x < 1) then 1 else x * f (x-1)) |]
        in run _seriD_fix [s| _s factorial 5 |],
    "foo decl" ~: IntegerE 42 ~=? run [] _seriC_foo,
    "fact5 decl" ~: IntegerE 120 ~=? run _seriD_fact5 _seriC_fact5,
    "subctx" ~: IntegerE 720 ~=? run _seriD_fact6 _seriC_fact6,
    "rfact5" ~: IntegerE 120 ~=? run _seriD_rfact [s| rfact 5 |],
    "id id 5" ~: IntegerE 5 ~=? run _seriD_id [s| (id id) 5 |],
    "justInteger" ~: IntegerE 5 ~=? run _seriD_fromMaybeInteger
            [s| fromMaybeInteger 10 (JustInteger 5) |],
    "noInteger" ~: IntegerE 10 ~=? run _seriD_fromMaybeInteger
            [s| fromMaybeInteger 10 NoInteger |],
    "just Bool" ~: trueE ~=? run _seriD_fromMaybeBool
            [s| fromMaybeBool False (Just True) |],
    "no Bool" ~: falseE ~=? run _seriD_fromMaybeBool
            [s| fromMaybeBool False Nothing |]
    ]

