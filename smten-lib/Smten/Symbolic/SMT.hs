
{-# LANGUAGE NoImplicitPrelude #-}

-- | A monad for repeated SMT queries.
module Smten.Symbolic.SMT
{-# DEPRECATED "Use Smten.Searches instead" #-}
   (SMT, query, runSMT) where

import Smten.Prelude
import Smten.Symbolic0
import Smten.Searches

type SMT = Searches

-- | Run the symbolic computation and get the result.
-- This does not effect the current context.
query :: Symbolic a -> SMT (Maybe a)
query = search

-- | Run an SMT monad with the given solver
runSMT :: Solver -> SMT a -> IO a
runSMT = runSearches

