
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Debug.Trace0 (trace, traceS) where

import qualified Debug.Trace as P
import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

trace :: String -> a -> a
trace = P.trace

-- | Print the symbolic representation of the first argument, then return
-- the second.
{-# NOINLINE traceS #-}
traceS :: a -> b -> b
traceS _ x = x

