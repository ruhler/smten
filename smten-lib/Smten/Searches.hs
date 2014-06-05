
{-# LANGUAGE NoImplicitPrelude #-}

-- | A monad for repeated searches.
module Smten.Searches (
    Searches, search, runSearches,
    module Smten.Search,
  ) where

import Smten.Prelude
import Smten.Search hiding (search)
import qualified Smten.Search
import Smten.Control.Monad.Reader

type Searches = ReaderT Solver IO

-- | Run a search cand get the result.
-- This does not effect the current context.
search :: Space a -> Searches (Maybe a)
search x = do
  s <- ask
  liftIO $ Smten.Search.search s x

-- | Run searches with the given solver
runSearches :: Solver -> Searches a -> IO a
runSearches s m = runReaderT m s

