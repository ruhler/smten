
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.MiniSat (main) where

import Smten.Prelude
import qualified Smten.Tests.SMT.Core as Core
import qualified Smten.Tests.SMT.Datatype as Datatype
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.MiniSat

main :: IO ()
main = do
    runtest (SMTTestCfg minisat [] ["SMT.Core.Not"]) Core.smttests
    putStrLn "MiniSat.SMT.Core PASSED"

    runtest (SMTTestCfg minisat [] []) Datatype.smttests
    putStrLn "MiniSat.SMT.Datatype PASSED"

