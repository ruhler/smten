
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Char0 (char_eq) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool

char_eq :: Char -> Char -> Bool
char_eq a b = if a P.== b then True else False

