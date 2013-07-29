
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.Exit (exitSuccess) where

import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

exitSuccess :: IO a
exitSuccess = primitive "Smten.System.Exit.exitSuccess"

