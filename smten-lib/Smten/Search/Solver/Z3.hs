
{-# LANGUAGE NoImplicitPrelude #-}
-- | This module provides the z3 solver for smten.
module Smten.Search.Solver.Z3 (z3) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic0

{-# ANN module PrimitiveModule #-}

-- | The z3 smten solver.
z3 :: Solver
z3 = primitive "Smten.Symbolic.Solver.Z3.z3"

