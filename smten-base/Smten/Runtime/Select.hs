
module Smten.Runtime.Select(
    select,
    ) where

import Control.Exception
import System.IO.Unsafe

-- | Run select on the given arguments, returning the approximation of any
-- argument which did not finish.
--
-- TODO: This currently waits until both results either evaluate to weak head
-- normal form or raise an error. In the future we may want (need?) to instead
-- evaluate the arguments concurrently and only wait a finite amount of time
-- after the first result to finish before giving up on the second result.
select :: a -> b -> a -> b -> (a, b)
select x_approx y_approx x_ y_ = unsafeDupablePerformIO $ do
  mx <- trysync (evaluate x_)
  my <- trysync (evaluate y_)
  case (mx, my) of
    (Right x, Right y) -> return (x, y)
    (Right x, Left ey) -> return (x, y_approx)
    (Left ex, Right y) -> return (x_approx, y)
    (Left ex, Left ey) -> throwIO ex

-- Run try, catching only synchronous exceptions.
-- We don't want to catch asynchronous exceptions, because then we can do user
-- interrupts with Ctrl-C, which makes profiling and other things miserable.
-- TODO: should we catch stack overflow?
trysync :: IO a -> IO (Either SomeException a)
trysync = tryJust notAsync

notAsync :: SomeException -> Maybe SomeException
notAsync e =
  case (fromException e :: Maybe AsyncException) of
     Just {} -> Nothing -- don't catch asynchronous exceptions
     Nothing -> Just e  -- catch everything else

