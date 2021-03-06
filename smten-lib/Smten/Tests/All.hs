
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.All (main) where

import Smten.Prelude
import Smten.Tests.Test
import qualified Smten.Tests.Array
import qualified Smten.Tests.Basic
import qualified Smten.Tests.Bit
import qualified Smten.Tests.Char
import qualified Smten.Tests.DataMap
import qualified Smten.Tests.List
import qualified Smten.Tests.Prelude
import qualified Smten.Tests.Prim
import qualified Smten.Tests.State
import qualified Smten.Tests.SMT.Core
import qualified Smten.Tests.SMT.Datatype
import qualified Smten.Tests.SMT.Opt
import qualified Smten.Tests.Trace

-- Import things so that they will be compiled by the plugin, even though we
-- don't actually have tests to run for them.
import qualified Smten.Control.Monad.Error ()
import qualified Smten.Control.Monad.State.Strict ()
import qualified Smten.Search ()
import qualified Smten.Searches ()
import qualified Smten.Symbolic ()
import qualified Smten.Symbolic.SMT ()
import qualified Smten.System.Environment ()
import qualified Smten.System.Exit ()
import qualified Smten.System.Timeout ()
import qualified Smten.Tests.SMT.Bit ()
import qualified Smten.Tests.SMT.Error ()
import qualified Smten.Tests.SMT.Integer ()

main :: IO ()
main = do
  test "Dummy" True
  Smten.Tests.Basic.tests
  Smten.Tests.Prelude.tests
  Smten.Tests.Char.tests
  Smten.Tests.Prim.tests
  Smten.Tests.Bit.tests
  Smten.Tests.DataMap.tests
  Smten.Tests.List.tests
  Smten.Tests.Array.tests
  Smten.Tests.State.tests
  Smten.Tests.Trace.tests
  Smten.Tests.SMT.Core.tests
  Smten.Tests.SMT.Datatype.tests
  Smten.Tests.SMT.Opt.tests
  putStrLn "ALL PASSED"

