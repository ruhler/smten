
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.DataMap (tests) where

import Smten.Prelude
import qualified Smten.Data.Map as Map
import Smten.Tests.Test

m1 :: Map.Map Integer Integer
m1 = Map.fromList [(5, 3), (1, 4), (6, 2), (8, 9)]

tests :: IO ()
tests = do
    test "DataMap.lookup_yes" (Map.lookup 1 m1 == Just 4)
    test "DataMap.lookup_no" (Map.lookup 4 m1 == Nothing)
    test "DataMap.insert" (Map.lookup 4 (Map.insert 4 17 m1) == Just 17)
    putStrLn "DataMap PASSED"

