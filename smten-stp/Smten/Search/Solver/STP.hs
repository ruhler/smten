
{-# LANGUAGE NoImplicitPrelude #-}
-- | This module provides the STP backend for smten.
module Smten.Search.Solver.STP (stp) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Search.Prim

{-# ANN module PrimitiveModule #-}

-- | The STP smten solver
stp :: Solver
stp = primitive "Smten.Search.Solver.STP.stp"


