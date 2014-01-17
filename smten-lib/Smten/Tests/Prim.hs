
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Prim (tests, f) where

import Smten.Prelude
import Smten.Tests.Test

f :: Int -> (# Int, Int #)
f x = (# x+1, x-1 #)

tests :: IO ()
tests = do
   test "UnboxedTuple" $
     case (f 4) of
         (# a, b #) -> a + b == 8

   putStrLn "Prim PASSED"

