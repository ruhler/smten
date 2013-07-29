
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char0 (char_eq, char_leq, ord, chr) where

import Data.Char
import qualified Prelude as P
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

char_eq :: Char -> Char -> Bool
char_eq a b = a P.== b

char_leq :: Char -> Char -> Bool
char_leq a b = a P.<= b


