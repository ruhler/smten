
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Test (test) where

import Smten.Prelude

test :: String -> Bool -> IO ()
test _ True = return ()
test msg False = error msg

