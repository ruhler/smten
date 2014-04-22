
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.Char (tests) where

import Smten.Prelude
import Smten.Data.Char
import Smten.Tests.Test

tests :: IO ()
tests = do
  test "Char.ord" (97 == ord 'a')
  test "Char.chr" (chr 97 == 'a')

  putStrLn "Char PASSED"

