
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Prim (tests, f) where

import Smten.Prelude
import Smten.Tests.Test

-- TODO: Handle this case, which requires ite of kind #.
--f :: Int -> (# Int, Int #)
--f x = case (x < 2) of
--        True -> (# x+1, x-1 #) 
--        False -> (# x+2, x-2 #)

f :: Int -> (# Int, Int #)
f x = (# x+1, x-1 #) 

tests :: IO ()
tests = do
   test "UnboxedTuple" $
     case (f 4) of
         (# a, b #) -> a + b == 8

   putStrLn "Prim PASSED"

