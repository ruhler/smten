
module Smten.Strict (modifyS, (<$!>)) where

import Control.Monad.State

-- | Strict modify
modifyS :: (MonadState s m) => (s -> s) -> m ()
modifyS f = do
    s <- get
    put $! f s

(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
(<$!>) f x = do
    v <- x
    return $! f v

