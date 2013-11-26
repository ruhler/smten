
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Yices2 (main) where

import Smten.Prelude
import qualified Smten.Tests.SMT.Core as Core
import qualified Smten.Tests.SMT.Datatype as Datatype
import qualified Smten.Tests.SMT.Error as Error
import qualified Smten.Tests.SMT.Integer as Integer
import qualified Smten.Tests.SMT.Bit as Bit
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Yices2

main :: IO ()
main = do
    runtest (SMTTestCfg yices2 ["SMT.Core.Integer"] []) Core.smttests
    putStrLn "Yices2.SMT.Core PASSED"

    runtest (SMTTestCfg yices2 ["Datatype.Enum", "Datatype.Struct", "Datatype.Mix", "Datatype.Caseoflet"] []) Datatype.smttests
    putStrLn "Yices2.SMT.Datatype PASSED"

    --runtest (SMTTestCfg yices2 [] []) Integer.smttests
    --putStrLn "Yices2.SMT.Integer PASSED"
    putStrLn "Yices2.SMT.Integer SKIPPED"

    --runtest (SMTTestCfg yices2 [] []) Bit.smttests
    --putStrLn "Yices2.SMT.Bit PASSED"
    putStrLn "Yices2.SMT.Bit SKIPPED"

    runtest (SMTTestCfg yices2 ["SMT.Error.Bool",
                                "SMT.Error.Integer",
                                "SMT.Error.Bit"] []) Error.smttests
    putStrLn "Yices2.SMT.Error PASSED"

