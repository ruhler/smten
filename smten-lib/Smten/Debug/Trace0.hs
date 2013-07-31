
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Debug.Trace0 (trace) where

import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

trace :: String -> a -> a
trace = primitive "Smten.Debug.Trace0.trace"

