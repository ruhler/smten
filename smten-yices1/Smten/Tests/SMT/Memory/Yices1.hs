
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Memory.Yices1 (main) where

import Smten.Prelude
import Smten.Tests.SMT.Memory
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Yices1

main :: IO ()
main = runtest (SMTTestCfg yices1 []) memtest

