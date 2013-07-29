
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.Environment (getArgs) where

import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

getArgs :: IO [String]
getArgs = primitive "Smten.System.Environment.getArgs"

