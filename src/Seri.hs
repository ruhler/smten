
{-# LANGUAGE QuasiQuotes #-}

module Seri (
    module Seri.Elaborate,
    module Seri.IR,
    module Seri.Quoter,
    module Seri.Typed,
    tests
    ) where

import Seri.Elaborate
import Seri.IR
import Seri.Quoter
import Seri.Typed

import Test.HUnit


-- Test Cases for Seri

run :: [Dec] -> TypedExp a -> Exp
run decls = elaborate decls . typed

[s|
    foo :: Integer
    foo = (\x -> x*x+3*x+2) 5
|]


[s|
    factorial :: Integer -> Integer
    factorial = fix (\f -> \x -> if (x < 1) then 1 else x * f (x-1))
|]

[s|
    fact5 :: Integer
    fact5 = factorial 5
|]

[s|
    fact6 :: Integer
    fact6 = 6 * fact5
|]

[s|
    rfact :: Integer -> Integer
    rfact = \x -> if (x < 1) then 1 else x * rfact (x-1)
|]

tests = "Seri" ~: [
    "foo" ~: IntegerE 42 ~=? run [] [s|(\x -> x*x+3*x+2) 5|],
    "true" ~: PrimE BoolT TrueP ~=? run _serictx_True [s| True |],
    "if" ~: IntegerE 23 ~=? run [] [s| if 6 < 4 then 42 else 23 |],
    "slice" ~: IntegerE 7 ~=? run [] [s| 3 + @(integerE . toInteger $ length [4,1,5,56]) |],
    "fix" ~: IntegerE 120 ~=?
        let factorial = [s| fix (\f -> \x -> if (x < 1) then 1 else x * f (x-1)) |]
        in run _serictx_fix [s| @(factorial) 5 |],
    "foo decl" ~: IntegerE 42 ~=? run [] _seri__foo,
    "fact5 decl" ~: IntegerE 120 ~=? run _serictx_fact5 _seri__fact5,
    "subctx" ~: IntegerE 720 ~=? run _serictx_fact6 _seri__fact6,
    "rfact5" ~: IntegerE 120 ~=? run _serictx_rfact [s| rfact 5 |]
    ]
