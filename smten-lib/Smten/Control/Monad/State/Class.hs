
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Control.Monad.State.Class where

import Smten.Prelude

class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
    s <- get
    put (f s)

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

