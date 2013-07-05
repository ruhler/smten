
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Eq0 (int_eq, integer_eq) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool

int_eq :: Int -> Int -> Bool
int_eq a b = if a P.== b then True else False

integer_eq :: Integer -> Integer -> Bool
integer_eq a b = if a P.== b then True else False

