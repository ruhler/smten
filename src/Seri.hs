
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

-- foo: (\x -> x*x + 3*x + 2) 5
foo :: Exp
foo =
 let x = VarE IntegerT "x"
     body = AddE (AddE (MulE x x) (MulE (IntegerE 3) x)) (IntegerE 2)
     lam = LamE (ArrowT IntegerT IntegerT) "x" body
 in AppE IntegerT lam (IntegerE 5)

rn :: TypedExp a -> Exp
rn = elaborate . typed

tests = "Seri" ~: [
    "Elaborate" ~: [
        "simple" ~: IntegerE 42 ~=? elaborate foo
        ],
    "Typed" ~: [
        "lt" ~: BoolE True ~=?
            let texp = ltE (integerE 4) (integerE 29)
                exp = typed texp
            in elaborate exp,
        "if" ~: IntegerE 12 ~=?
            let texp = ifE (boolE False) (integerE 10) (integerE 12)
                exp = typed texp
            in elaborate exp,
        "foo" ~: IntegerE 42 ~=?
            let body = \x -> addE (addE (mulE x x) (mulE (integerE 3) x)) (integerE 2)
                lam = lamE "x" body
                texp = appE lam (integerE 5)
                exp = typed texp
            in elaborate exp
        ],
    "General" ~: [
        "simple" ~: IntegerE 42 ~=? rn [st|(\x -> x*x+3*x+2) 5|],
        "true" ~: BoolE True ~=? rn [st| true |],
        "if" ~: IntegerE 23 ~=? rn [st| if 6 < 4 then 42 else 23 |],
        "slice" ~: IntegerE 7 ~=? rn [st| 3 + @(integerE . toInteger $ length [4,1,5,56]) |],
        "fix" ~: IntegerE 120 ~=?
            let factorial = [st| !f \x -> if (x < 1) then 1 else x * f (x-1) |]
            in rn [st| @(factorial) 5 |]
        ]
    ]

