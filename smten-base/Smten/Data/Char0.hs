
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char0 (char_eq, ord) where

import Data.Char
import qualified Prelude as P
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

char_eq :: Char -> Char -> Bool
char_eq a b = if a P.== b then True else False 


