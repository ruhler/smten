
module Smten.SMT.FreeID (FreeID, fresh, freenm) where

import System.IO.Unsafe
import Data.IORef

type FreeID = Integer

{-# NOINLINE freshpool #-}
freshpool :: IORef FreeID
freshpool = unsafePerformIO $ newIORef 0

fresh :: IO FreeID
fresh = do
  v <- readIORef freshpool
  modifyIORef' freshpool (+ 1)
  return v

freenm :: FreeID -> String
freenm x = "f~" ++ show x

