
{-# LANGUAGE NoImplicitPrelude #-}
-- | This module provides the z3 solver for smten.
module Smten.Symbolic.Solver.Z3
  {-# DEPRECATED "Use Smten.Search.Solver.Z3 instead" #-}
    (z3) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- | The z3 smten solver.
z3 :: Solver
z3 = primitive "Smten.Symbolic.Solver.Z3.z3"

