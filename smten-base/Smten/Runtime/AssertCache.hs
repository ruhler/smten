
{-# LANGUAGE PatternGuards #-}

-- | Per-formula 1-element cache for the Smten.Runtime.Assert traversal 
module Smten.Runtime.AssertCache (
    AssertCache, AssertCacheKey, new, newKey, cached,
    ) where

import Control.Monad
import Data.IORef
import Data.Unique
import GHC.Base (Any)
import System.IO.Unsafe
import Unsafe.Coerce

type AssertCacheKey = Unique
newtype AssertCache = AssertCache (IORef (Maybe (AssertCacheKey, Any)))

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
--
-- It is up to the user to ensure the type 'a' is fixed for a given
-- cache and key.
cached :: AssertCache -> AssertCacheKey -> IO a -> IO a
cached (AssertCache cache) key action = do
  incache <- readIORef cache
  let iscached = {-# SCC "IsCached" #-} do
        (oldkey, x) <- incache
        guard (key == oldkey)
        return $! unsafeCoerce x
  case iscached of
    Just v -> return v
    Nothing -> do
        value <- action
        {-# SCC "Cache_update" #-} writeIORef cache (Just (key, unsafeCoerce value))
        return value

