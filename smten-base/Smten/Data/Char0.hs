
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char0 (char_eq, char_leq, ord, chr) where

import qualified Data.Char as P
import qualified Prelude as P
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

char_eq :: P.Char -> P.Char -> Bool
char_eq a b = a P.== b

char_leq :: P.Char -> P.Char -> Bool
char_leq a b = a P.<= b

ord :: P.Char -> P.Int
ord = P.ord

chr :: P.Int -> P.Char
chr = P.chr


