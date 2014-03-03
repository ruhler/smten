
{-# LANGUAGE PatternGuards #-}

-- | Per-formula 1-element cache for the Smten.Runtime.Build traversal 
module Smten.Runtime.BuildCache (
    BuildCache, BuildCacheKey, new, newKey, cached,
    ) where

import Data.IORef
import GHC.Base (Any)
import System.IO.Unsafe
import Unsafe.Coerce

type BuildCacheKey = Integer
data CacheEntry = Cached BuildCacheKey Any
newtype BuildCache = BuildCache (IORef CacheEntry)

instance Show BuildCache where
    show _ = "?BuildCache?"

empty :: CacheEntry
empty = Cached 0 (error "BuildCache: empty!")

-- | Create a new cache.
-- You supply the constructor function, and it is called with a new cache
-- to compute the result.
new :: (BuildCache -> a) -> a
new f = unsafeDupablePerformIO $ do
          c <- newIORef empty
          return (f (BuildCache c))

-- | Create a new cache key
-- To use a cached value, it must have the same key as last time you
-- computed it.
newKey :: IO BuildCacheKey
newKey = do
  r <- atomicModifyIORef keySource $ \x -> let z = x+1 in (z,z)
  r `seq` return r

-- Perform the IO operation and cache the result, but only if the
-- result isn't already in the cache.
--
-- It is up to the user to ensure the type 'a' is fixed for a given
-- cache and key.
cached :: BuildCache -> BuildCacheKey -> IO a -> IO a
cached (BuildCache cache) key action = do
  entry <- readIORef cache
  case entry of
    Cached oldkey x
      | oldkey == key -> return $! unsafeCoerce x
      | otherwise -> do
            v <- action
            writeIORef cache (Cached key (unsafeCoerce v))
            return v

keySource :: IORef BuildCacheKey
keySource = unsafePerformIO (newIORef 1)
{-# NOINLINE keySource #-}

