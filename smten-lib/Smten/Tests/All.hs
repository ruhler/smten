
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.All (main) where

import Smten.Prelude
import Smten.Tests.Test
import qualified Smten.Tests.Array
import qualified Smten.Tests.Basic
import qualified Smten.Tests.Bit
import qualified Smten.Tests.DataMap
import qualified Smten.Tests.State
import qualified Smten.Tests.SMT.Bit ()
import qualified Smten.Tests.SMT.Core
import qualified Smten.Tests.SMT.Datatype
import qualified Smten.Tests.SMT.Integer ()
import qualified Smten.Tests.SMT.Sudoku ()

main :: IO ()
main = do
  test "Dummy" True
  Smten.Tests.Basic.tests
  Smten.Tests.Bit.tests
  Smten.Tests.DataMap.tests
  Smten.Tests.Array.tests
  Smten.Tests.State.tests
  Smten.Tests.SMT.Core.tests
  Smten.Tests.SMT.Datatype.tests
  putStrLn "ALL PASSED"

