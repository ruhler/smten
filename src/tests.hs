
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import System.Exit

import Seri.Arithmetic
import Seri.Elaborate
import Seri.Lambda
import Seri.IR

data MyType = MyIntegerT IntegerT
            | MyArrowT (ArrowT MyType)
        deriving(Show, Eq)

data MyExp = MyIntegerE IntegerE
           | MyAddE (AddE MyExp)
           | MyMulE (MulE MyExp)
           | MyVarE (VarE MyType)
           | MyLamE (LamE MyType MyExp)
           | MyAppE (AppE MyType MyExp)
        deriving(Show, Eq)

ir ''MyType ''MyExp

four :: AddE MyExp
four = AddE (inject $ IntegerE 1) (inject $ IntegerE 3)

eight :: AddE MyExp
eight = AddE (inject four) (inject four)

twelve :: MulE MyExp
twelve = MulE (inject $ IntegerE 3) (inject $ IntegerE 4)

sixteen :: AddE MyExp
sixteen = AddE (inject four) (inject twelve)

my :: (Inject e MyExp) => e -> MyExp
my = inject

myt :: (Inject t MyType) => t -> MyType
myt = inject

-- foo: (\x -> x*x + 3*x + 2) 5
foo :: MyExp
foo =
 let x = my $ VarE (myt IntegerT) "x"
     body = my $ AddE (my $ AddE (my $ MulE x x) (my $ MulE (my $ IntegerE 3) x)) (my $ IntegerE 2)
     lam = my $ LamE (myt $ ArrowT (myt IntegerT) (myt IntegerT)) "x" body 
 in my $ AppE (myt IntegerT) lam (my $ IntegerE 5)

tests = "Tests" ~: [
        "dummy" ~: 4 ~=? 4,
        "four" ~: (inject (IntegerE 4) :: MyExp) ~=? elaborate four,
        "eight" ~: (inject (IntegerE 8) :: MyExp) ~=? elaborate eight,
        "twelve" ~: (inject (IntegerE 12) :: MyExp) ~=? elaborate twelve,
        "sixteen" ~: (inject (IntegerE 16) :: MyExp) ~=? elaborate sixteen,
        "foo" ~: (inject (IntegerE 42) :: MyExp) ~=? elaborate foo
    ]

-- Run tests, exiting failure if any failed, exiting success if all succeeded.
runtests :: Test -> IO ()
runtests t = do
    cnts <- runTestTT t
    putStrLn $ show cnts
    if (errors cnts + failures cnts > 0)
        then exitFailure
        else exitWith ExitSuccess

main :: IO ()
main = runtests tests

