
-- | Fast equality by pointer comparison.
module Smten.Runtime.StableNameEq (
    stableNameEq,
 ) where

import System.IO.Unsafe
import System.Mem.StableName

    
-- | Return true if the two arguments point to the same location in the heap.
-- If this returns True, it means the arguments are one and the same.
stableNameEq :: a -> a -> Bool
stableNameEq x y = unsafeDupablePerformIO $ do
    xnm <- makeStableName x
    ynm <- makeStableName y
    return (xnm == ynm)

