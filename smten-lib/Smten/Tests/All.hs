
module Smten.Tests.All (main) where

import Smten.Tests.Test
import qualified Smten.Tests.Basic

main :: IO ()
main = do
  test "Dummy" True
  Smten.Tests.Basic.tests
  putStrLn "ALL PASSED"

