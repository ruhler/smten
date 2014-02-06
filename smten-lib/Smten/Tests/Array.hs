
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.Array (tests) where

import Smten.Prelude
import Smten.Data.Array
import Smten.Tests.Test
import Smten.Data.Bit

arr :: Array Integer Integer
arr = array (0, 2) [(0, 42), (1, 12), (2, 19)]

arr2 :: Array (Bit 3) Integer
arr2 = array (0, 2) [(0, 42), (1, 12), (2, 19)]

tests :: IO ()
tests = do
  test "Array.sub0" (42 == (arr ! 0))
  test "Array.sub1" (12 == (arr ! 1))
  test "Array.sub2" (19 == (arr ! 2))
  test "Array.upd" (23 == ((arr // [(1, 23)]) ! 1))

  test "array.fmap" (True == ((fmap (== 12) arr) ! 1))

  test "Array.sub0.bit" (42 == (arr2 ! 0))
  test "Array.sub1.bit" (12 == (arr2 ! 1))
  test "Array.sub2.bit" (19 == (arr2 ! 2))
  test "Array.upd.bit" (23 == ((arr2 // [(1, 23)]) ! 1))


  putStrLn "Array PASSED"

