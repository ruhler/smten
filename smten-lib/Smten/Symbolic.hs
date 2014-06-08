
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides the Symbolic monad, for orchestration of symbolic
-- computations.
module Smten.Symbolic
  {-# DEPRECATED "Use Smten.Search instead" #-}
  (
    Symbolic, Solver, run_symbolic,
    MonadPlus(..),
    free_Bool, free_Integer, free_Bit, assert,
    Free(..),
    ) where

import Smten.Prelude
import Smten.Search.Prim
import Smten.Search

-- | Assert the given predicate is satisfied.
assert :: Bool -> Space ()
assert p = if p then return () else mzero

