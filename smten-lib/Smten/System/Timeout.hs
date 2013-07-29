
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.Timeout (timeout) where

import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

timeout :: Int -> IO a -> IO (Maybe a)
timeout = primitive "Smten.System.Timeout.timeout"

