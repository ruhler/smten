
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Debug.Trace (trace, traceShow) where

import Smten.Prelude
import Smten.Debug.Trace0

traceShow :: (Show a) => a -> b -> b
traceShow x = trace (show x)

