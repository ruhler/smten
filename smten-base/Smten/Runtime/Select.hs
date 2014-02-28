
module Smten.Runtime.Select(
    SelectResult(..), select, approximate,
    ) where

import Control.Exception
import System.IO.Unsafe

data SelectResult = SRBoth | SRLeft | SRRight

-- | Evaluate both arguments concurrently, waiting for at least one of them to
-- reach weak head normal form, and maybe some finite amount of time after
-- that for the other to reach weak head normal form.
--
-- Returns which of the arguments reached weak head normal form.
select :: a -> b -> SelectResult
select x y = unsafeDupablePerformIO (selectIO x y)

-- | Run select on the given arguments, returning the approximation of any
-- argument which did not finish.
approximate :: a -> b -> a -> b -> (a, b)
approximate da db a b = 
   case select a b of
     SRLeft -> (a, db)
     SRRight -> (da, b)
     SRBoth -> (a, b)

-- TODO: This currently waits until both results either evaluate to weak head
-- normal form or raise an error. In the future we may want (need?) to instead
-- evaluate the arguments concurrently and only wait a finite amount of time
-- after the first result to finish before giving up on the second result.
selectIO :: a -> b -> IO SelectResult
selectIO x_ y_ = do
  mx <- trysync (evaluate x_)
  my <- trysync (evaluate y_)
  case (mx, my) of
    (Right x, Right y) -> return SRBoth
    (Right x, Left ey) -> return SRLeft
    (Left ex, Right y) -> return SRRight
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

