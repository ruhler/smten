
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.System.IO0 (
    module Smten.System.IO0
  ) where

import Smten.Runtime.SmtenHS
import Smten.System.IO0
import Smten.Compiled.Smten.Smten.Base

instance SmtenHS1 IO where
    error1 = error "TODO: IO.error1"
    realize1 = error "TODO: IO.realize1"
    ite1 = error "TODO: IO.ite1"

