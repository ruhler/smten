
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.Timeout (timeout) where

import qualified Prelude as P
import qualified System.Timeout as P
import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

timeout :: Int -> IO a -> IO (Maybe a)
timeout a b = do
  r <- P.timeout a b
  return $ case r of
             P.Just x -> Just x
             P.Nothing -> Nothing

