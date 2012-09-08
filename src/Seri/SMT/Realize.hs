
module Seri.SMT.Realize (
   Realize(..)
   ) where

import Data.Functor

-- This implementation is meant to be visible to query back ends, but not
-- query users. That's why we define it here instead of in Query.hs. Query
-- users should NOT import this module.
data Realize q a = Realize {
    runRealize :: q a
}

instance (Functor q) => Functor (Realize q) where
    fmap f x = Realize (f <$> runRealize x)

instance (Monad q) => Monad (Realize q) where
    fail = Realize . fail
    return = Realize . return
    (>>=) (Realize x) f = Realize $ do
        v <- x
        runRealize (f v)

