
module Smten.Runtime.Select(SelectResult(..), select) where

import Prelude hiding(Either(..))
import qualified Prelude as P
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

data SelectResult a b = Both a b | Left a | Right b

-- | Evaluate both arguments concurrently, waiting for at least one of them to
-- reach weak head normal form, and maybe some finite amount of time after
-- that for the other to reach weak head normal form.
--
-- The values returned have been evaluated to weak head normal form.
select :: a -> b -> SelectResult a b
select x y = unsafePerformIO (selectIO x y)

data Result = Done | Err SomeException

data Side = L | R
    deriving (Eq, Show)

data Message = Message Side Result

selectIO :: a -> b -> IO (SelectResult a b)
selectIO x y = do
  mvar <- newEmptyMVar

  -- Concurrently evaluate the left argument
  forkFinally (evaluate x) $ \r ->
    case r of
        P.Left e -> putMVar mvar (Message L (Err e))
        P.Right _ -> putMVar mvar (Message L Done)

  -- Concurrently evaluate the right argument
  forkFinally (evaluate y) $ \r ->
    case r of
        P.Left e -> putMVar mvar (Message R (Err e))
        P.Right _ -> putMVar mvar (Message R Done)

  -- Wait for the first result.
  r1 <- takeMVar mvar
  case r1 of
     Message _ (Err e1) -> do
        -- The first result has no answer. Wait for the other to finish.
        r2 <- takeMVar mvar
        case r2 of
            Message s2 (Err e2) -> throwIO e1
            Message L Done -> return (Left x) 
            Message R Done -> return (Right y)
     Message s1 Done -> do
        -- The first answer has a result.
        -- Check if the other result is also ready, then return.
        -- TODO: do we want to wait a little longer for the other?
        --   Either with 'threadDelay' or more calls to yield?
        yield
        mr2 <- tryTakeMVar mvar
        case (s1, mr2) of
          (_, Just (Message _ Done)) -> return (Both x y)
          (R, Just (Message _ (Err {}))) -> return (Right y)
          (L, Just (Message _ (Err {}))) -> return (Left x)
          (R, Nothing) -> return (Right y)
          (L, Nothing) -> return (Left x)
            
