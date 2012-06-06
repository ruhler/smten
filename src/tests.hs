
import Test.HUnit
import System.Exit

import qualified Seri.Tests
import qualified Seri.Lambda.Parser.Tests

-- Run tests, exiting failure if any failed, exiting success if all succeeded.
runtests :: Test -> IO ()
runtests t = do
    cnts <- runTestTT t
    putStrLn $ show cnts
    if (errors cnts + failures cnts > 0)
        then exitFailure
        else exitWith ExitSuccess

tests = "Tests" ~: [
    Seri.Lambda.Parser.Tests.tests,
    Seri.Tests.tests
    ]

main :: IO ()
main = runtests tests

