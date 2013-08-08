
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Memory.Z3 (main) where

import Smten.Prelude
import Smten.Tests.SMT.Memory
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Z3

main :: IO ()
main = runtest (SMTTestCfg z3 []) memtest

