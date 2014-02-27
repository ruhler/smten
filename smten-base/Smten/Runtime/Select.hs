
{-# OPTIONS_GHC -auto-all #-}

module Smten.Runtime.Select(
    SelectResult(..), select, approximate,
    ) where

import Prelude hiding(Either(..))
import qualified Prelude as P
import Control.Exception
import System.IO.Unsafe

data SelectResult a b = Both a b | Left a | Right b

-- | Evaluate both arguments concurrently, waiting for at least one of them to
-- reach weak head normal form, and maybe some finite amount of time after
-- that for the other to reach weak head normal form.
--
-- The values returned have been evaluated to weak head normal form.
select :: a -> b -> SelectResult a b
select x y = unsafeDupablePerformIO (selectIO x y)

-- | Run select on the given arguments, returning the approximation of any
-- argument which did not finish.
approximate :: a -> b -> a -> b -> (a, b)
approximate da db a_ b_ = 
   case select a_ b_ of
     Left a -> (a, db)
     Right b -> (da, b)
     Both a b -> (a, b)

-- TODO: This currently waits until both results either evaluate to weak head
-- normal form or raise an error. In the future we may want (need?) to instead
-- evaluate the arguments concurrently and only wait a finite amount of time
-- after the first result to finish before giving up on the second result.
selectIO :: a -> b -> IO (SelectResult a b)
selectIO x_ y_ = do
  mx <- trysync (evaluate x_)
  my <- trysync (evaluate y_)
  case (mx, my) of
    (P.Right x, P.Right y) -> return (Both x y)
    (P.Right x, P.Left ey) -> return (Left x)
    (P.Left ex, P.Right y) -> return (Right y)
    (P.Left ex, P.Left ey) -> throwIO ex

-- Run try, catching only synchronous exceptions.
-- We don't want to catch asynchronous exceptions, because then we can do user
-- interrupts with Ctrl-C, which makes profiling and other things miserable.
-- TODO: should we catch stack overflow?
trysync :: IO a -> IO (P.Either SomeException a)
trysync = tryJust notAsync

notAsync :: SomeException -> Maybe SomeException
notAsync e =
  case (fromException e :: Maybe AsyncException) of
     Just {} -> Nothing -- don't catch asynchronous exceptions
     Nothing -> Just e  -- catch everything else

