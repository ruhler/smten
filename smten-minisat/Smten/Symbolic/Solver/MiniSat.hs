
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a MiniSat backend for smten.
module Smten.Symbolic.Solver.MiniSat
  {-# DEPRECATED "Use Smten.Search.Solver.MiniSat instead" #-}
  (minisat) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- | The MiniSat Solver
minisat :: Solver
minisat = primitive "Smten.Symbolic.Solver.MiniSat.minisat"

