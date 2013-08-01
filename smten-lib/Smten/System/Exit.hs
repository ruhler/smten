
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.Exit (exitSuccess) where

import qualified Prelude as P
import qualified System.Exit as P
import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

exitSuccess :: IO a
exitSuccess = P.exitSuccess

