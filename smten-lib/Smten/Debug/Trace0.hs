
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Debug.Trace0 (trace) where

import qualified Debug.Trace as P
import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

trace :: String -> a -> a
trace = P.trace

