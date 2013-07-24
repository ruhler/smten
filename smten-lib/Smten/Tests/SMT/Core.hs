
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Core (tests) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.Solver.Pure
import Smten.Symbolic.Solver.Debug
import Smten.Tests.SMT.Test

tests :: IO ()
tests = do
   symtesteq "SMT.Core.Trivial" (Just ()) [pure] (return ())
   symtesteq "SMT.Core.Fail" Nothing [pure] $ (mzero :: Symbolic ())

   symtesteq "SMT.Core.FreeBool" (Just True) [pure] $ do
      p <- free_Bool
      assert p
      return p

   symtesteq "SMT.Core.FreeBool2" (Just ()) [pure] $ do
        p <- free_Bool
        if p
           then return ()
           else mzero

   symtesteq "SMT.Core.JoinMaybe" (Just (Just ())) [pure] $ do
      p <- free_Bool
      assert p
      return $ if p then Just () else Nothing

   -- Verify we can handle functions in let expressions.
   symtesteq "SMT.Core.Finlet" (Just ()) [pure] $ do
       assert (let f = (\x -> x) in f True)

   -- Test NOT
   symtesteq "SMT.Core.Not" (Just False) [pure] $ do
       b <- free_Bool
       assert (not b)
       return b

   -- Test OR
   symtesteq "SMT.Core.Or" (Just (False, True)) [pure] $ do
       c <- free_Bool
       d <- free_Bool
       assert (c || d)
       assert (not c)
       return (c, d)

   -- Test AND
   symtesteq "SMT.Core.And" (Just (True, True)) [pure] $ do
       e <- free_Bool
       f <- free_Bool
       assert (e && f)
       return (e, f)

   -- Test EQ
   symtesteq "SMT.Core.Eq" (Just (False, False)) [pure] $ do
       g <- free_Bool
       h <- free_Bool
       assert (g == h)
       assert (not g)
       return (g, h)

   -- Test IF
   symtesteq "SMT.Core.If" (Just False) [pure] $ do
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
   symtest "SMT.Core.Complex" tstcmplx [debug "complex.dbg" pure] $ do
       j <- free_Bool
       k <- free_Bool
       l <- free_Bool
       assert (p j k l)
       return (j, k, l)

   -- Test an issue with lambdas that we've had issues with in the past.
   symtesteq "SMT.Core.Lambda" (Just False) [pure] $ do
       m <- free_Bool
       assert ((if m then (==) True else (==) False) False)
       return m

   -- We should be able to using Integers, so long as they aren't free, even
   -- if the underlying solver doesn't support them.
   symtesteq "SMT.Core.Integer" (Just (False, True)) [pure] $ do
       n <- free_Bool
       o <- free_Bool
       assert ((if n then 3 else (4 :: Integer)) == (if o then 4 else 5))
       return (n, o)

   -- Same with lists
   symtesteq "SMT.Core.List" (Just False) [pure] $ do
       p <- free_Bool
       assert (null (if p then [1, 2, 3 :: Integer] else []))
       return p

   -- Same with char
   symtesteq "SMT.Core.Char" (Just True) [pure] $ do
       q <- free_Bool
       assert ('a' == (if q then 'a' else 'b'))
       return q

   -- Test that primitives are reapplied after substitution.
   symtesteq "SMT.Core.Substitute" (Just True) [pure] $ do
       a <- free_Bool
       assert (not a)
       return (not a)

   -- Test conversion of symbolic Symbolic to concrete Symbolic.
   symtesteq "SMT.Core.Symsym" (Just (False, False)) [pure] $ do
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
     

