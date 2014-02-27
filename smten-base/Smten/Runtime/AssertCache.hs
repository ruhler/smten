
{-# LANGUAGE PatternGuards #-}

-- | Per-formula 1-element cache for the Smten.Runtime.Assert traversal 
module Smten.Runtime.AssertCache (
    AssertCache, AssertCacheKey, new, newKey, cached,
    ) where

import Data.IORef
import Data.Unique
import GHC.Base (Any)
import System.IO.Unsafe
import Unsafe.Coerce

type AssertCacheKey = Unique
data CacheEntry = Empty | Cached AssertCacheKey Any
newtype AssertCache = AssertCache (IORef CacheEntry)

instance Show AssertCache where
    show _ = "?AssertCache?"

-- | Create a new cache.
-- You supply the constructor function, and it is called with a new cache
-- to compute the result.
new :: (AssertCache -> a) -> a
new f = {-# SCC "AssertCache_new" #-} unsafePerformIO $ do
          c <- newIORef Empty
          return (f (AssertCache c))

-- | Create a new cache key
-- To use a cached value, it must have the same key as last time you
-- computed it.
newKey :: IO AssertCacheKey
newKey = newUnique

-- Perform the IO operation and cache the result, but only if the
-- result isn't already in the cache.
--
-- It is up to the user to ensure the type 'a' is fixed for a given
-- cache and key.
cached :: AssertCache -> AssertCacheKey -> IO a -> IO a
cached (AssertCache cache) key action = do
  entry <- readIORef cache
  case entry of
    Cached oldkey x | oldkey == key -> return $! unsafeCoerce x
    _ -> do
        v <- action
        {-# SCC "Cache_update" #-} writeIORef cache (Cached key (unsafeCoerce v))
        return v

