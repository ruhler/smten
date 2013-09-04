
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.SMC (tests) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.Solver.Pure
import Smten.SMC.SMC
import Smten.SMC.ShiftReg


-- TODO: Have this fail if it gives the wrong answer.
tests :: IO ()
tests = do
   s <- run_symbolic pure (check shiftregm shiftregf 3)
   putStrLn $ show s

