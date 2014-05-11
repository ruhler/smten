
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.Environment (getArgs) where

import qualified System.Environment as P
import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

getArgs :: IO [String]
getArgs = P.getArgs

