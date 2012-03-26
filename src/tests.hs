
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

import Test.HUnit
import System.Exit

import Seri.Arithmetic
import Seri.Elaborate
import Seri.Lambda

data MyExp = MyInteger IntegerE
           | MyAdd (AddE MyExp)
           | MyMul (MulE MyExp)
           | MyVar VarE
           | MyLam (LamE MyExp)
           | MyApp (AppE MyExp)
        deriving(Show, Eq)

instance Inject (AppE MyExp) MyExp where
    inject = MyApp
    unject (MyApp x) = Just x
    unject _ = Nothing

instance Inject (LamE MyExp) MyExp where
    inject = MyLam
    unject (MyLam x) = Just x
    unject _ = Nothing

instance Inject VarE MyExp where
    inject = MyVar
    unject (MyVar x) = Just x
    unject _ = Nothing

instance Inject (MulE MyExp) MyExp where
    inject = MyMul
    unject (MyMul x) = Just x
    unject _ = Nothing

instance Inject (AddE MyExp) MyExp where
    inject = MyAdd
    unject (MyAdd x) = Just x
    unject _ = Nothing

instance Inject IntegerE MyExp where
    inject = MyInteger
    unject (MyInteger x) = Just x
    unject _ = Nothing

instance Elaborate MyExp MyExp where
    elaborate (MyInteger x) = elaborate x
    elaborate (MyAdd x) = elaborate x
    elaborate (MyMul x) = elaborate x
    elaborate (MyVar x) = elaborate x
    elaborate (MyApp x) = elaborate x
    elaborate (MyLam x) = elaborate x
    reduce n v (MyInteger x) = reduce n v x
    reduce n v (MyAdd x) = reduce n v x
    reduce n v (MyMul x) = reduce n v x
    reduce n v (MyVar x) = reduce n v x
    reduce n v (MyApp x) = reduce n v x
    reduce n v (MyLam x) = reduce n v x

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

-- foo: (\x -> x*x + 3*x + 2) 5
foo :: MyExp
foo =
 let x = my $ VarE "x"
     body = my $ AddE (my $ AddE (my $ MulE x x) (my $ MulE (my $ IntegerE 3) x)) (my $ IntegerE 2)
     lam = my $ LamE "x" body 
 in my $ AppE lam (my $ IntegerE 5)

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

