
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Debug.Trace (trace, traceS, traceSS, traceShow) where

import Smten.Prelude
import Smten.Debug.Trace0

traceShow :: (Show a) => a -> b -> b
traceShow x = trace (show x)

-- traceS with a string prefixed to the string.
traceSS :: String -> a -> b -> b
traceSS msg x y = trace msg (traceS x y)

