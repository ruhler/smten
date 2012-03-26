
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
run :: (Monad m) => Exp -> m Exp
run exp = do
    let inferred = typeinfer exp
    typecheck inferred
    return $ elaborate inferred

is :: Exp -> Either String Exp
is = Right

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
        "simple" ~: is (IntegerE 42) ~=? run [s|(\x -> x*x+3*x+2) 5|],
        "frontspace" ~: is (IntegerE 13) ~=? run [s| 8+5|],
        "backspace" ~: is (IntegerE 13) ~=? run [s|8+5 |],
        "space" ~: is (IntegerE 13) ~=? run [s| 8 + 5 |],
        "subtract" ~: is (IntegerE 3) ~=? run [s| 8 - 5 |],
        "true" ~: is (BoolE True) ~=? run [s| true |],
        "false" ~: is (BoolE False) ~=? run [s| false |],
        "lt" ~: is (BoolE False) ~=? run [s| 6 < 4 |],
        "if" ~: is (IntegerE 23) ~=? run [s| if 6 < 4 then 42 else 23 |],
        "fix" ~: is (IntegerE 120) ~=?
            let factorial = [s| !f \x -> if (x < 1) then 1 else x * f (x-1) |]
            in run [s| @(factorial) 5 |]
        ]
    ]

