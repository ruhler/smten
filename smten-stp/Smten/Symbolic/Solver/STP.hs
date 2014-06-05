
{-# LANGUAGE NoImplicitPrelude #-}
-- | This module provides the STP backend for smten.
module Smten.Symbolic.Solver.STP
  {-# DEPRECATED "Use Smten.Search.Solver.STP instead" #-}
 (stp) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- | The STP smten solver
stp :: Solver
stp = primitive "Smten.Symbolic.Solver.STP.stp"

