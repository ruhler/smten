
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Memory.Pure (main) where

import Smten.Prelude
import Smten.Tests.SMT.Memory
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Pure

main :: IO ()
main = runtest (SMTTestCfg pure []) memtest

