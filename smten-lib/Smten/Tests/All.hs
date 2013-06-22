
module Smten.Tests.All (main) where

import Smten.Tests.Test

main :: IO ()
main = do
  test "Dummy" True
  putStrLn "ALL PASSED"

