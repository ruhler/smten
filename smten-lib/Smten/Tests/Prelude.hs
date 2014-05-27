
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.Prelude (tests, tgc) where

import Smten.Prelude
import Smten.Tests.Test

-- Verify we can at least link to getContents
tgc :: Bool -> IO String
tgc True = return "hi"
tgc False = getContents

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
  test "p.quotRem.Int.p" ((10, 4) == quotRem (74 :: Int) 7)
  test "p.quotRem.Int.n" ((-10, 4) == quotRem (74 :: Int) (-7))
  test "p.divMod.Int.p" ((10, 4) == divMod (74 :: Int) 7)
  test "p.divMod.Int.n" ((-11, -3) == divMod (74 :: Int) (-7))
  test "p.read.Int" ((10 :: Int) == read "10")
  test "p.read.Integer" ((10 :: Integer) == read "10")
  test "p.Show.Maybe" ("Just (Just Nothing)" == show (Just (Just (Nothing :: Maybe Bool))))
  test "p.Show.Unit" ("()" == show ())
  test "p.Show.Tuple2" ("(True,False)" == show (True,False))
  test "p.Show.Tuple3" ("(True,False,True)" == show (True,False,True))
  test "p.Show.Tuple4" ("(True,False,True,False)" == show (True,False,True,False))

  x <- tgc True
  test "p.tgc" (x == "hi")

  putStrLn "Prelude PASSED"

