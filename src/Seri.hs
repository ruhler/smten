
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

run :: TypedExp a -> Exp
run = elaborate . typed

tests = "Seri" ~: [
    "foo" ~: IntegerE 42 ~=? run [st|(\x -> x*x+3*x+2) 5|],
    "true" ~: BoolE True ~=? run [st| true |],
    "if" ~: IntegerE 23 ~=? run [st| if 6 < 4 then 42 else 23 |],
    "slice" ~: IntegerE 7 ~=? run [st| 3 + @(integerE . toInteger $ length [4,1,5,56]) |],
    "fix" ~: IntegerE 120 ~=?
        let factorial = [st| !f \x -> if (x < 1) then 1 else x * f (x-1) |]
        in run [st| @(factorial) 5 |]
    ]

