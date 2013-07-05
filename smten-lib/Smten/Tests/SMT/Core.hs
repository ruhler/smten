
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Core (tests) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Tests.SMT.Test

tests :: IO ()
tests = do
   symtesteq "SMT.Core.Trivial" (Just ()) [Pure] (return ())
   symtesteq "SMT.Core.Fail" Nothing [Pure] $ (mzero :: Symbolic ())

   symtesteq "SMT.Core.FreeBool" (Just True) [Pure] $ do
      p <- free_Bool
      assert p
      return p

   symtesteq "SMT.Core.FreeBool2" (Just ()) [Pure] $ do
        p <- free_Bool
        if p
           then return ()
           else mzero

   symtesteq "SMT.Core.JoinMaybe" (Just (Just ())) [Pure] $ do
      p <- free_Bool
      assert p
      return $ if p then Just () else Nothing

   -- Verify we can handle functions in let expressions.
   symtesteq "Core.Finlet" (Just ()) [Pure] $ do
       assert (let f = (\x -> x) in f True)

   -- Test NOT
   symtesteq "Core.Not" (Just False) [Pure] $ do
       b <- free_Bool
       assert (not b)
       return b

   -- Test OR
   symtesteq "Core.Or" (Just (False, True)) [Pure] $ do
       c <- free_Bool
       d <- free_Bool
       assert (c || d)
       assert (not c)
       return (c, d)

   -- Test AND
   symtesteq "Core.And" (Just (True, True)) [Pure] $ do
       e <- free_Bool
       f <- free_Bool
       assert (e && f)
       return (e, f)

   -- Test EQ
   symtesteq "Core.Eq" (Just (False, False)) [Pure] $ do
       g <- free_Bool
       h <- free_Bool
       assert (g == h)
       assert (not g)
       return (g, h)

   -- Test IF
   symtesteq "Core.If" (Just False) [Pure] $ do
       i <- free_Bool
       assert (if i then False else True)
       return i

   -- Test more complex
   let p x y z =
           if x && y
               then not z
               else x == (y || z)

       tstcmplx (Just (jv, kv, lv)) = p jv kv lv
       tstcmplx _ = False
   symtest "Core.Complex" tstcmplx [Pure] $ do
       j <- free_Bool
       k <- free_Bool
       l <- free_Bool
       assert (p j k l)
       return (j, k, l)

   -- Test an issue with lambdas that we've had issues with in the past.
   symtesteq "Core.Lambda" (Just False) [Pure] $ do
       m <- free_Bool
       assert ((if m then (==) True else (==) False) False)
       return m

   -- We should be able to using Integers, so long as they aren't free, even
   -- if the underlying solver doesn't support them.
   symtesteq "Core.Integer" (Just (False, True)) [Pure] $ do
       n <- free_Bool
       o <- free_Bool
       assert ((if n then 3 else (4 :: Integer)) == (if o then 4 else 5))
       return (n, o)

   -- Same with lists
   symtesteq "Core.List" (Just False) [Pure] $ do
       p <- free_Bool
       assert (null (if p then [1, 2, 3 :: Integer] else []))
       return p

   -- Same with char
   symtesteq "Core.Char" (Just True) [Pure] $ do
       q <- free_Bool
       assert ('a' == (if q then 'a' else 'b'))
       return q

   -- Test that primitives are reapplied after substitution.
   symtesteq "Core.Substitute" (Just True) [Pure] $ do
       a <- free_Bool
       assert (not a)
       return (not a)

   -- Test conversion of symbolic Symbolic to concrete Symbolic.
   symtesteq "Core.Symsym" (Just (False, False)) [Pure] $ do
       a <- free_Bool
       b <- free_Bool
       if a
           then do
               assert b
               assert False
           else do
               assert (not b)
       return (a, b)
   
   putStrLn "SMT.Core PASSED"
     

