
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic.Solver.Debug (debug) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- Solver which writes the queries to the given file, then calls the given
-- solver to solve them.
debug :: FilePath -> Solver -> Solver
debug = primitive "Smten.Symbolic.Solver.Debug.debug"

