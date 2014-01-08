
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Opt (tests) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.Solver.Smten
import Smten.Tests.SMT.Test

-- If the given argument is concretely True, this returns True, otherwise it
-- generates an infinitely large result.
-- This is used to test for optimizations which are needed to recognize when
-- things that look symbolic are actually concrete.
isTrueConcrete :: Bool -> Bool
isTrueConcrete p = if p then True else (not p) && isTrueConcrete p

-- SMT query optimization tests.
-- The user should inspect the generated debug files to verify queries are
-- optimized as required.
smttests :: SMTTest ()
smttests = do
   -- We should not introduce any symbolic variables from redundant mzero.
   symtesteq "SMT.Opt.PlusZero" (Just ()) $ do
      p <- mplus (return True) mzero
      assert (isTrueConcrete p)

   -- We should not introduce any symbolic variables from the mplus,
   -- because we have no mzeros and no _|_.
   symtesteq "SMT.Opt.PlusSelf" (Just ()) $ do
      u <- mplus (return ()) (return ())
      assert (isTrueConcrete (u == ()))

   symtesteq "SMT.Opt.PlusSelfInt" (Just ()) $ do
      x <- mplus (return (1 :: Int)) (return (1 :: Int))
      assert (isTrueConcrete (x <= 5))

   symtesteq "SMT.Opt.PlusSelfInteger" (Just ()) $ do
      x <- mplus (return (1 :: Integer)) (return (1 :: Integer))
      assert (isTrueConcrete (x <= 5))

   symtesteq "SMT.Opt.PlusUnreachableInteger" (Just ()) $ do
      p <- free_Bool
      let x = if p then Just (1 :: Integer) else Nothing
      case x of
        Just v -> assert (isTrueConcrete (v <= 5))
        Nothing -> return ()
   
tests :: IO ()
tests = runtest (SMTTestCfg smten [] []) smttests

