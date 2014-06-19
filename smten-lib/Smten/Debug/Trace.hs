
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Debug.Trace (trace, traceS, traceShow) where

import Smten.Prelude
import Smten.Debug.Trace0

traceShow :: (Show a) => a -> b -> b
traceShow x = trace (show x)

