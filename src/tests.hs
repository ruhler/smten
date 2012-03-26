
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Test.HUnit
import System.Exit

import Seri.Arithmetic
import Seri.Elaborate

data MyExp = MyInteger IntegerE
           | MyAdd (AddE MyExp)
        deriving(Show, Eq)

instance Inject (AddE MyExp) MyExp where
    inject = MyAdd
    unject (MyAdd x) = Just x
    unject _ = Nothing

instance Inject IntegerE MyExp where
    inject = MyInteger
    unject (MyInteger x) = Just x
    unject _ = Nothing

instance Elaborate MyExp MyExp where
    elaborate = id

four :: AddE MyExp
four = AddE (IntegerE 1) (IntegerE 3)

tests = "Tests" ~: [
        "dummy" ~: 4 ~=? 4,
        "four" ~: (inject (IntegerE 4) :: MyExp) ~=? elaborate four
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

