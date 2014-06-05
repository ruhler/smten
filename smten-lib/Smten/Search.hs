
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides the Space monad.
-- An alternate presentation of the Smten API for serach
module Smten.Search (
  Space, Solver, search,
  empty, single, union,
  guard,
  ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Symbolic

type Space = Symbolic

empty :: Space a
empty = mzero 

single :: a -> Space a
single = return

union :: Space a -> Space a -> Space a
union = mplus

search :: Solver -> Space a -> IO (Maybe a)
search = run_symbolic

