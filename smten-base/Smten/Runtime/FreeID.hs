
module Smten.Runtime.FreeID (
    FreeID, freenm, fresh, withfresh,
    ) where

import Data.IORef
import System.IO.Unsafe

type FreeID = Integer

-- TODO: Maybe we could use a more compact representation, like base 52, to
-- make shorter variable names? Would it be prettier? More efficient?
freenm :: FreeID -> String
freenm x = {-# SCC "FreeName" #-} "f~" ++ show x

-- Return a globally fresh variable.
withfresh :: (FreeID -> a) -> a
withfresh f = unsafeDupablePerformIO $ do
    nm <- fresh
    return (f nm) 

freshSource :: IORef FreeID
freshSource = unsafePerformIO (newIORef 0)
{-# NOINLINE freshSource #-}

fresh :: IO FreeID
fresh = do
  r <- atomicModifyIORef freshSource $ \x -> let z = x+1 in (z,z)
  r `seq` return r

