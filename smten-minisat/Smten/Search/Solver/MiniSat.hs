
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a MiniSat backend for smten.
module Smten.Search.Solver.MiniSat (minisat) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic0

{-# ANN module PrimitiveModule #-}

-- | The MiniSat Solver
minisat :: Solver
minisat = primitive "Smten.Search.Solver.MiniSat.minisat"

