
import Test.HUnit
import System.Exit

import qualified Seri
import qualified Seri.SMT

-- Run tests, exiting failure if any failed, exiting success if all succeeded.
runtests :: Test -> IO ()
runtests t = do
    cnts <- runTestTT t
    putStrLn $ show cnts
    if (errors cnts + failures cnts > 0)
        then exitFailure
        else exitWith ExitSuccess

tests = "Tests" ~: [
    Seri.tests,
    Seri.SMT.tests
    ]

main :: IO ()
main = runtests tests

