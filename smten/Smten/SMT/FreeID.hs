
module Smten.SMT.FreeID (
    FreeID, fresh, freenm,
    Assignment,
    ) where

import System.IO.Unsafe
import Data.IORef

import Data.Dynamic

type FreeID = Integer

type Assignment = [(FreeID, Dynamic)]

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

