
{-# LANGUAGE NoImplicitPrelude #-}

-- | A monad for repeated SMT queries.
module Smten.Symbolic.SMT (SMT, query, runSMT) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Control.Monad.Reader

type SMT = ReaderT Solver IO

-- | Run the symbolic computation and get the result.
-- This does not effect the current context.
query :: Symbolic a -> SMT (Maybe a)
query x = do
  s <- ask
  liftIO $ run_symbolic s x

-- | Run an SMT monad with the given solver
runSMT :: Solver -> SMT a -> IO a
runSMT s m = runReaderT m s

