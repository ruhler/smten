
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Smten.Control.Monad0 (Monad(..)) where

import Smten.Smten.Base
import Smten.System.IO0

-- Note: this definition must match up with the definition from
-- GHC.Base
class Monad m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (>>) :: forall a b. m a -> m b -> m b
    return :: a -> m a
    fail :: String -> m a

    {-# INLINE (>>) #-}
    m >> k = m >>= \_ -> k
    fail s = error s

instance Monad IO where
    return = return_io
    (>>=) = bind_io

