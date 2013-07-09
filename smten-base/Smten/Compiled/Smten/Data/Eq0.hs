
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Eq0 (int_eq, integer_eq) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool

int_eq :: Int -> Int -> Bool
int_eq a b = if a P.== b then True else False

integer_eq :: Integer -> Integer -> Bool
integer_eq a b = if a P.== b then True else False

