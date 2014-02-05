
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.Test (test) where

import Smten.Prelude

test :: String -> Bool -> IO ()
test msg p =
  case p of
    True -> return ()
    False -> error msg

