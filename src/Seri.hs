
{-# LANGUAGE QuasiQuotes #-}

module Seri (
    module Seri.Parser,
    module Seri.Quoter,
    module Seri.TypeCheck,
    module Seri.TypeInfer,
    module Seri.IR,
    module Seri.Elaborate,
    tests
    ) where

import Seri.Parser
import Seri.Quoter
import Seri.TypeCheck
import Seri.TypeInfer
import Seri.IR
import Seri.Elaborate

import Test.HUnit


-- Test Cases for Seri

-- foo: (\x -> x*x + 3*x + 2) 5
foo :: Exp
foo =
 let x = VarE IntegerT "x"
     body = AddE (AddE (MulE x x) (MulE (IntegerE 3) x)) (IntegerE 2)
     lam = LamE (ArrowT IntegerT IntegerT) "x" body
 in AppE IntegerT lam (IntegerE 5)

-- Run type inference and type checking on the expression,
-- Then elaborate it.
run :: Exp -> Maybe Exp
run exp = do
    let inferred = typeinfer exp
    typecheck inferred
    return $ elaborate inferred

tests = "Seri" ~: [
    "Elaborate" ~: [
        "simple" ~: IntegerE 42 ~=? elaborate foo
        ],
    "Quoter" ~: [
        "simple" ~: IntegerE 42 ~=? elaborate [s|(\x -> x*x+3*x+2) 5|],
        "slice" ~:  IntegerE 47 ~=? elaborate [s|(\x -> x*x+@(toInteger $ length [1, 2, 3, 4])*x+2) 5|],
        "nested" ~: IntegerE 142 ~=? 
            let muln :: Integer -> Exp -> Exp
                muln 1 x = x
                muln n x = MulE x $ muln (n-1) x

                t = muln 3 [s|x|]
            in elaborate [s|(\x -> @(t)+3*x+2) 5|]
        ],
    "TypeCheck" ~: [
        "simple good" ~: Just IntegerT ~=? typecheck foo,
        "bad_app" ~: Nothing ~=?
            let body = VarE IntegerT "x"
                lam = LamE (ArrowT IntegerT IntegerT) "x" body
                exp = AppE (ArrowT IntegerT IntegerT) lam (IntegerE 5)
            in typecheck exp,
        "uninferred" ~: Nothing ~=? typecheck [s|(\x -> x+2)|]
        ],
    "TypeInfer" ~: [
        "simple" ~: foo ~=? typeinfer [s|(\x -> x*x+3*x+2) 5|]
        ],
    "General" ~: [
        "simple" ~: Just (IntegerE 42) ~=? run [s|(\x -> x*x+3*x+2) 5|],
        "frontspace" ~: Just (IntegerE 13) ~=? run [s| 8+5|],
        "backspace" ~: Just (IntegerE 13) ~=? run [s|8+5 |],
        "space" ~: Just (IntegerE 13) ~=? run [s| 8 + 5 |],
        "subtract" ~: Just (IntegerE 3) ~=? run [s| 8 - 5 |],
        "true" ~: Just (BoolE True) ~=? run [s| true |],
        "false" ~: Just (BoolE False) ~=? run [s| false |]
        ]
    ]

