
module Smten.Runtime.RunBoth (
    runBoth, RunBothResult(..)
 ) where

import Control.Concurrent
import Control.Monad
import Data.Functor
import Data.Maybe

data RunBothResult a = OneFinished a (IO a)
                     | BothFinished a a

data RunBothConfig =
   Immediate       -- ^ Return as soon as the first result is ready
 | Wait Int        -- ^ Wait at most n milliseconds after first result is ready
 | BlockInOrder    -- ^ Run both to completion in order
 | BlockConcurrent -- ^ Run both to completion concurrently

runBothConfig :: RunBothConfig
runBothConfig = Wait 10000
--runBothConfig = BlockInOrder
--runBothConfig = BlockConcurrent
--runBothConfig = Immediate

-- Run two IO computations together.
--   Waits for at least one result to finish.
runBoth :: IO a -> IO a -> IO (RunBothResult a)
runBoth = case runBothConfig of
            Immediate -> runBothImmediate
            Wait n -> runBothWait n
            BlockInOrder -> runBothBlockInOrder
            BlockConcurrent -> runBothBlockConcurrent

runBothBlockInOrder :: IO a -> IO a -> IO (RunBothResult a)
runBothBlockInOrder = liftM2 BothFinished

runBothBlockConcurrent :: IO a -> IO a -> IO (RunBothResult a)
runBothBlockConcurrent a b = do
  mvar <- newEmptyMVar
  forkIO (a >>= putMVar mvar)
  forkIO (b >>= putMVar mvar)
  liftM2 BothFinished (takeMVar mvar) (takeMVar mvar)

runBothImmediate :: IO a -> IO a -> IO (RunBothResult a)
runBothImmediate a b = do
  mvar <- newEmptyMVar
  forkIO (a >>= putMVar mvar)
  forkIO (b >>= putMVar mvar)

  s1 <- takeMVar mvar
  ms2 <- tryTakeMVar mvar

  case ms2 of
    Just s2 -> return $ BothFinished s1 s2
    Nothing -> return $ OneFinished s1 (takeMVar mvar)


runBothWait :: Int -> IO a -> IO a -> IO (RunBothResult a)
runBothWait wait a b = do
  mvar <- newEmptyMVar
  forkIO (a >>= putMVar mvar . Just)
  forkIO (b >>= putMVar mvar . Just)

  -- Wait for the one to finish.
  -- The result is guarenteed to be 'Just'
  s1 <- fromJust <$> takeMVar mvar

  -- Wait at most a little more time for the other to finish
  forkIO (threadDelay wait >> putMVar mvar Nothing)
  ms2 <- takeMVar mvar

  case ms2 of
    Just s2 -> return $ BothFinished s1 s2
    Nothing -> return $ OneFinished s1 (fromJust <$> takeMVar mvar)

