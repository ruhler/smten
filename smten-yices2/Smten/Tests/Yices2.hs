
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Yices2 (main) where

import Smten.Prelude
import qualified Smten.Tests.SMT.Core as Core
import qualified Smten.Tests.SMT.Datatype as Datatype
import Smten.Tests.SMT.Test
import Smten.Symbolic.Solver.Yices2

main :: IO ()
main = do
    runtest yices2 Core.smttests
    putStrLn "Yices2.SMT.Core PASSED"

    runtest yices2 Datatype.smttests
    putStrLn "Yices2.SMT.Datatype PASSED"

