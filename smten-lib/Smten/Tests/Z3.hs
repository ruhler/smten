
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Z3 (main) where

import Smten.Prelude
import qualified Smten.Tests.SMT.Core as Core
import qualified Smten.Tests.SMT.Datatype as Datatype
import qualified Smten.Tests.SMT.Error as Error
import qualified Smten.Tests.SMT.Integer as Integer
import qualified Smten.Tests.SMT.Bit as Bit
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Z3

main :: IO ()
main = do
    runtest (SMTTestCfg z3 [] []) Core.smttests
    putStrLn "Z3.SMT.Core PASSED"

    runtest (SMTTestCfg z3 ["Datatype.Enum", "Datatype.Struct", "Datatype.Mix", "Datatype.Caseoflet"] []) Datatype.smttests
    putStrLn "Z3.SMT.Datatype PASSED"

    runtest (SMTTestCfg z3 [] []) Integer.smttests
    putStrLn "Z3.SMT.Integer PASSED"

    runtest (SMTTestCfg z3 [] []) Bit.smttests
    putStrLn "Z3.SMT.Bit PASSED"

    runtest (SMTTestCfg z3 ["SMT.Error.Bool",
                            "SMT.Error.Integer",
                            "SMT.Error.Bit"] []) Error.smttests
    putStrLn "Z3.SMT.Error PASSED"

