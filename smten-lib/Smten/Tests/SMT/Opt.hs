
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Opt (tests) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.Solver.Smten
import Smten.Tests.SMT.Test

-- SMT query optimization tests.
-- The user should inspect the generated debug files to verify queries are
-- optimized as required.
smttests :: SMTTest ()
smttests = do
   -- SMT.Opt.0
   -- This should generate a query with assert 'True'
   -- We should not introduce any symbolic variables from the mplus,
   -- because we have no mzeros and no _|_.
   symtesteq "SMT.Opt.0" (Just ()) $ mplus (return ()) (return ())

   -- SMT.Opt.1
   -- This should generate a query with assert 'True'
   -- We should not introduce any symbolic variables from redundant mzero.
   symtesteq "SMT.Opt.1" (Just ()) $ do
      p <- mplus (return True) mzero
      case p of
        True -> return ()
        False -> return ()
   
tests :: IO ()
tests = do
   runtest (SMTTestCfg smten [] ["SMT.Opt.0", "SMT.Opt.1"]) smttests
   putStrLn "SMT.Opt: Check debug output to see if properly optimized"

