
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Memory.STP (main) where

import Smten.Prelude
import Smten.Tests.SMT.Memory
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.STP

main :: IO ()
main = runtest (SMTTestCfg stp []) memtest

