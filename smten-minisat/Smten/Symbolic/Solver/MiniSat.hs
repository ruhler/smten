
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

-- | This module provides a MiniSat backend for smten.
module Smten.Symbolic.Solver.MiniSat (minisat) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- | The MiniSat Solver
minisat :: Solver
minisat = primitive "Smten.Symbolic.Solver.MiniSat.minisat"

