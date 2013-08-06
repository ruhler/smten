
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.STP (main) where

import Smten.Prelude
import qualified Smten.Tests.SMT.Core as Core
import qualified Smten.Tests.SMT.Datatype as Datatype
import qualified Smten.Tests.SMT.Bit as Bit
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.STP

main :: IO ()
main = do
    runtest (SMTTestCfg stp []) Core.smttests
    putStrLn "STP.SMT.Core PASSED"

    runtest (SMTTestCfg stp []) Datatype.smttests
    putStrLn "STP.SMT.Datatype PASSED"

    runtest (SMTTestCfg stp []) Bit.smttests
    putStrLn "STP.SMT.Bit PASSED"

