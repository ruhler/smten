
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Runtime.Select(SelectResult(..), select) where

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
select x y = unsafePerformIO (selectIO x y)

-- TODO: This currently waits until both results either evaluate to weak head
-- normal form or raise an error. In the future we may want (need?) to instead
-- evaluate the arguments concurrently and only wait a finite amount of time
-- after the first result to finish before giving up on the second result.
selectIO :: forall a b . a -> b -> IO (SelectResult a b)
selectIO x_ y_ = do
  mx <- try (evaluate x_) :: IO (P.Either SomeException a)
  my <- try (evaluate y_) :: IO (P.Either SomeException b)
  case (mx, my) of
    (P.Right x, P.Right y) -> return (Both x y)
    (P.Right x, P.Left ey) -> return (Left x)
    (P.Left ex, P.Right y) -> return (Right y)
    (P.Left ex, P.Left ey) -> throwIO ex

