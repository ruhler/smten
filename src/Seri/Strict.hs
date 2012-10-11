
module Seri.Strict (modifyS) where

import Control.Monad.State

-- | Strict modify
modifyS :: (MonadState s m) => (s -> s) -> m ()
modifyS f = do
    s <- get
    put $! f s

