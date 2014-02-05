
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Control.Monad.Trans (
    MonadTrans(..),
    MonadIO(..),
  ) where

import Smten.Prelude

class MonadTrans t where
    lift :: Monad m => m a -> t m a

class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id
    
