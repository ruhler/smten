
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Yices1 (main) where

import Smten.Prelude
import qualified Smten.Tests.SMT.Core as Core
import qualified Smten.Tests.SMT.Datatype as Datatype
import qualified Smten.Tests.SMT.Integer as Integer
import qualified Smten.Tests.SMT.Bit as Bit
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Yices1

main :: IO ()
main = do
    runtest (SMTTestCfg yices1 []) Core.smttests
    putStrLn "Yices1.SMT.Core PASSED"

    runtest (SMTTestCfg yices1 []) Datatype.smttests
    putStrLn "Yices1.SMT.Datatype PASSED"

    runtest (SMTTestCfg yices1 []) Integer.smttests
    putStrLn "Yices1.SMT.Integer PASSED"

    runtest (SMTTestCfg yices1 ["SMT.Bit.Lsh"]) Bit.smttests
    putStrLn "Yices1.SMT.Bit PASSED"
