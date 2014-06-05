
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.Trace (tests) where

import Smten.Prelude
import Smten.Debug.Trace
import Smten.Tests.Test

tests :: IO ()
tests = do
  test "Trace.trace" (trace "Trace.trace says hi!" True)
  test "Trace.traceS" (traceS "TraceS says hi!" True)

  putStrLn "Trace PASSED"

