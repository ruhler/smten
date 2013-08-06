
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Memory.Yices2 (main) where

import Smten.Prelude
import Smten.Tests.SMT.Memory
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Yices2

main :: IO ()
main = runtest (SMTTestCfg yices2 []) memtest

