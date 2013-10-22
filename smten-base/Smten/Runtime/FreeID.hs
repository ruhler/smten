
module Smten.Runtime.FreeID (
    FreeID, fresh, freenm,
    ) where

import System.IO.Unsafe
import Data.IORef

type FreeID = Integer

{-# NOINLINE freshpool #-}
freshpool :: IORef FreeID
freshpool = unsafePerformIO $ newIORef 0

fresh :: IO FreeID
fresh = atomicModifyIORef' freshpool (\x -> (x+1, x))

freenm :: FreeID -> String
freenm x = "f~" ++ show x

