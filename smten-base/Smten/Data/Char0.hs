
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char0 (char_eq) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

char_eq :: Char -> Char -> Bool
char_eq a b = if a P.== b then True else False 


