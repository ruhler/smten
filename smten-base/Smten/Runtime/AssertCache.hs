
{-# LANGUAGE PatternGuards #-}

-- | Per-formula 1-element cache for the Smten.Runtime.Assert traversal 
module Smten.Runtime.AssertCache (
    AssertCache, AssertCacheKey, new, newKey, cached,
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Dynamic
import Data.IORef
import Data.Unique
import System.IO.Unsafe

type AssertCacheKey = Unique
newtype AssertCache = AssertCache (IORef (Maybe (AssertCacheKey, Dynamic)))

instance Show AssertCache where
    show _ = "?AssertCache?"

-- | Create a new cache.
-- You supply the constructor function, and it is called with a new cache
-- to compute the result.
new :: (AssertCache -> a) -> a
new f = {-# SCC "AssertCache_new" #-} unsafePerformIO $ do
          c <- newIORef Nothing
          return (f (AssertCache c))

-- | Create a new cache key
-- To use a cached value, it must have the same key as last time you
-- computed it.
newKey :: IO AssertCacheKey
newKey = newUnique

-- Perform the IO operation and cache the result, but only if the
-- result isn't already in the cache.
cached :: (MonadIO m, Typeable a) => AssertCache -> AssertCacheKey -> m a -> m a
cached (AssertCache cache) key action = do
  incache <- liftIO $ readIORef cache
  let iscached = {-# SCC "IsCached" #-} do
        (oldkey, dynamic) <- incache
        guard (key == oldkey)
        fromDynamic dynamic
  case iscached of
    Just v -> return v
    Nothing -> do
        value <- action
        liftIO $ {-# SCC "Cache_update" #-} writeIORef cache (Just (key, toDyn value))
        return value

