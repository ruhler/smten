
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.SMT.Core (smttests, tests) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Data.Array
import Smten.Search
import Smten.Search.Solver.Smten
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
   symtesteq "SMT.Core.Trivial" (Just ()) (return ())
   symtesteq "SMT.Core.Fail" Nothing $ (mzero :: Space ())
   symtesteq "SMT.Core.MPlusLeft" (Just True) $ mplus (return True) mzero
   symtesteq "SMT.Core.MPlusRight" (Just True) $ mplus mzero (return True)

   symtesteq "SMT.Core.FreeBool" (Just True) $ do
      p <- free_Bool
      guard p
      return p

   symtesteq "SMT.Core.FreeBool2" (Just ()) $ do
        p <- free_Bool
        if p
           then return ()
           else mzero

   symtesteq "SMT.Core.JoinMaybe" (Just (Just ())) $ do
      p <- free_Bool
      guard p
      return $ if p then Just () else Nothing

   -- Verify we can handle functions in let expressions.
   symtesteq "SMT.Core.Finlet" (Just ()) $ do
       guard (let f = (\x -> x) in f True)

   -- Test NOT
   symtesteq "SMT.Core.Not" (Just False) $ do
       b <- free_Bool
       guard (not b)
       return b

   -- Test OR
   symtesteq "SMT.Core.Or" (Just (False, True)) $ do
       c <- free_Bool
       d <- free_Bool
       guard (c || d)
       guard (not c)
       return (c, d)

   -- Test AND
   symtesteq "SMT.Core.And" (Just (True, True)) $ do
       e <- free_Bool
       f <- free_Bool
       guard (e && f)
       return (e, f)

   -- Test EQ
   symtesteq "SMT.Core.Eq" (Just (False, False)) $ do
       g <- free_Bool
       h <- free_Bool
       guard (g == h)
       guard (not g)
       return (g, h)

   -- Test IF
   symtesteq "SMT.Core.If" (Just False) $ do
       i <- free_Bool
       guard (if i then False else True)
       return i

   -- Test more complex
   let p x y z =
           if x && y
               then not z
               else x == (y || z)

       tstcmplx (Just (jv, kv, lv)) = p jv kv lv
       tstcmplx _ = False
   symtest "SMT.Core.Complex" tstcmplx $ do
       j <- free_Bool
       k <- free_Bool
       l <- free_Bool
       guard (p j k l)
       return (j, k, l)

   -- Test an issue with lambdas that we've had issues with in the past.
   symtesteq "SMT.Core.Lambda" (Just False) $ do
       m <- free_Bool
       guard ((if m then (==) True else (==) False) False)
       return m

   -- We should be able to using Integers, so long as they aren't free, even
   -- if the underlying solver doesn't support them.
   symtesteq "SMT.Core.Integer" (Just (False, True)) $ do
       n <- free_Bool
       o <- free_Bool
       guard ((if n then 3 else (4 :: Integer)) == (if o then 4 else 5))
       return (n, o)

   -- Same with lists
   symtesteq "SMT.Core.List" (Just False) $ do
       p <- free_Bool
       guard (null (if p then [1, 2, 3 :: Integer] else []))
       return p

   -- Same with char
   symtesteq "SMT.Core.Char" (Just True) $ do
       q <- free_Bool
       guard ('a' == (if q then 'a' else 'b'))
       return q

   -- Test that primitives are reapplied after substitution.
   symtesteq "SMT.Core.Substitute" (Just True) $ do
       a <- free_Bool
       guard (not a)
       return (not a)

   -- Test conversion of symbolic Symbolic to concrete Symbolic.
   symtesteq "SMT.Core.Symsym" (Just (False, False)) $ do
       a <- free_Bool
       b <- free_Bool
       if a
           then do
               guard b
               guard False
           else do
               guard (not b)
       return (a, b)
 
   -- Test an case we've had trouble with in the past
   -- The bug was we accidentally said:
   --   not (if p then a else b) = if p then b else a
   symtesteq "SMT.Core.DistinctInts" (Just ()) $ do
       [a, b, c] <- sequence $ replicate 3 (msum (map return [0, 1, 2 :: Int]))
       guard (a /= b && a /= c && b /= c)

   -- Test that we can use smten arrays with symbolic evaluation.
   symtesteq "SMT.Core.Array" (Just 2) $ do
       let arr :: Array Int Int
           arr = array (0, 2) [(0, 42), (1, 12), (2, 19)]
       idx <- msum (map return [0, 1, 2 :: Int])
       guard (arr ! idx == 19)
       return idx

   symtesteq "SMT.Core.Int.<.sc" (Just (2 :: Int)) $ do
       x <- mplus (return 2) (return 3)
       guard (x < 3)
       return x

   symtesteq "SMT.Core.Int.<.ss" (Just (1, 2 :: Int)) $ do
       x <- mplus (return 1) (return 2)
       y <- mplus (return 0) (return 2)
       guard (x < y)
       return (x, y)

   symtesteq "SMT.Core.Int.<=" (Just (2 :: Int)) $ do
       x <- mplus (return 2) (return 3)
       guard (x <= 2)
       return x

   symtesteq "SMT.Core.Int.<=.ss" (Just (2, 2 :: Int)) $ do
       x <- mplus (return 2) (return 3)
       y <- mplus (return 1) (return 2)
       guard (x <= y)
       return (x, y)

   symtesteq "SMT.Core.Int.==.ss" (Just (2, 2 :: Int)) $ do
       x <- mplus (return 2) (return 3)
       y <- mplus (return 1) (return 2)
       guard (x == y)
       return (x, y)

   symtesteq "SMT.Core.Int.>" (Just (3 :: Int)) $ do
       x <- mplus (return 2) (return 3)
       guard (x > 2)
       return x

   symtesteq "SMT.Core.Int.>=" (Just (3 :: Int)) $ do
       x <- mplus (return 2) (return 3)
       guard (x >= 3)
       return x

   symtesteq "SMT.Core.IteSym" (Just ()) $ do
       p <- union (single True) (single False)
       x <- if p 
               then single False    
               else union (single True) (single False)
       guard (x == x)

tests :: IO ()
tests = do
   runtest (SMTTestCfg smten [] []) smttests
   putStrLn "SMT.Core PASSED"

