
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic.Solver.Debug (debug) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- Solver which writes the queries to the given file, then calls the given
-- solver to solve them.
--
-- The solver may be reused for multiple queries; the debug output will be
-- appended to the file as needed.
debug :: FilePath -> Solver -> IO Solver
debug = primitive "Smten.Symbolic.Solver.Debug.debug"

