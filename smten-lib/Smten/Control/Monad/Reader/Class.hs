
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Control.Monad.Reader.Class (
    MonadReader(..),
    asks,
    ) where

import Smten.Control.Monad

class (Monad m) => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

asks :: (MonadReader r m) => (r -> a) -> m a
asks f = do
    r <- ask
    return (f r)

