
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Prelude (tests) where

import Smten.Prelude
import Smten.Tests.Test

tests :: IO ()
tests = do
  test "Prelude.foldl" ((1 :: Integer) == foldl (+) 0 [1])
  test "p.take" ([8 :: Integer, 3] == take 2 [8, 3, 5, 1])
  test "p.drop" ([5 :: Integer, 1] == drop 2 [8, 3, 5, 1])
  test "p.splitAt" (([8 :: Integer, 3], [5, 1]) == splitAt 2 [8, 3, 5, 1])
  test "p.concat" ([8 :: Integer, 3, 5, 1, 2, 5] == concat [[8, 3], [5], [1, 2, 5]])
  test "p.bang1" (([7 :: Integer, 2, 5, 3, 1] !! 0) == 7)
  test "p.bang2" (([7 :: Integer, 2, 5, 3, 1] !! 2) == 5)
  test "p.toEnum.Integer" ((4 :: Integer) == toEnum (4 :: Int))
  test "p.quotRem.Int" ((10, 4) == quotRem (74 :: Int) 7)
  test "p.read.Int" ((10 :: Int) == read "10")
  test "p.read.Integer" ((10 :: Integer) == read "10")
  test "p.Show.Maybe" ("Just (Just Nothing)" == show (Just (Just (Nothing :: Maybe Bool))))

  putStrLn "Prelude PASSED"

