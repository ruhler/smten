
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Yices2 (main) where

import Smten.Prelude
import Smten.Tests.SMT.Core
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Yices2

main :: IO ()
main = do
    runtest yices2 smttests
    putStrLn "Yices2.SMT.Core PASSED"

