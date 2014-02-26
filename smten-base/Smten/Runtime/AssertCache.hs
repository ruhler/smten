
{-# LANGUAGE PatternGuards #-}

-- | Per-formula 1-element cache for the Smten.Runtime.Assert traversal 
module Smten.Runtime.AssertCache (
    AssertCache, AssertCacheKey, new, newKey, cached,
    ) where

import Control.Monad
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
cached :: (Typeable a) => AssertCache -> AssertCacheKey -> IO a -> IO a
cached (AssertCache cache) key action = do
  incache <- readIORef cache
  let iscached = {-# SCC "IsCached" #-} do
        (oldkey, dynamic) <- incache
        guard (key == oldkey)
        fromDynamic dynamic
  case iscached of
    Just v -> return v
    Nothing -> do
        value <- action
        {-# SCC "Cache_update" #-} writeIORef cache (Just (key, toDyn value))
        return value

