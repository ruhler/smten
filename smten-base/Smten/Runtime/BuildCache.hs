
{-# LANGUAGE PatternGuards #-}

-- | Per-formula 1-element cache for the Smten.Runtime.Build traversal 
module Smten.Runtime.BuildCache (
    BuildCache, BuildCacheKey, new, newKey, cached,
    ) where

import Data.IORef
import Data.Unique
import GHC.Base (Any)
import System.IO.Unsafe
import Unsafe.Coerce

type BuildCacheKey = Unique
data CacheEntry = Empty | Cached BuildCacheKey Any
newtype BuildCache = BuildCache (IORef CacheEntry)

instance Show BuildCache where
    show _ = "?BuildCache?"

-- | Create a new cache.
-- You supply the constructor function, and it is called with a new cache
-- to compute the result.
new :: (BuildCache -> a) -> a
new f = unsafeDupablePerformIO $ do
          c <- newIORef Empty
          return (f (BuildCache c))

-- | Create a new cache key
-- To use a cached value, it must have the same key as last time you
-- computed it.
newKey :: IO BuildCacheKey
newKey = newUnique

-- Perform the IO operation and cache the result, but only if the
-- result isn't already in the cache.
--
-- It is up to the user to ensure the type 'a' is fixed for a given
-- cache and key.
cached :: BuildCache -> BuildCacheKey -> IO a -> IO a
cached (BuildCache cache) key action = do
  entry <- readIORef cache
  case entry of
    Cached oldkey x | oldkey == key -> return $! unsafeCoerce x
    _ -> do
        v <- action
        writeIORef cache (Cached key (unsafeCoerce v))
        return v

