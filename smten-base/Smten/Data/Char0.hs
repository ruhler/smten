
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char0 (char_eq, char_leq, ord, chr) where

import qualified Data.Char as P
import qualified Prelude as P
import Smten.Data.Bool
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE char_eq #-}
char_eq :: P.Char -> P.Char -> Bool
char_eq a b = {-# SCC "PRIM_CHAR_EQ" #-}  a P.== b

{-# NOINLINE char_leq #-}
char_leq :: P.Char -> P.Char -> Bool
char_leq a b = {-# SCC "PRIM_CHAR_LEQ" #-} a P.<= b

{-# NOINLINE ord #-}
ord :: P.Char -> P.Int
ord = {-# SCC "PRIM_ORD" #-} P.ord

{-# NOINLINE chr #-}
chr :: P.Int -> P.Char
chr = {-# SCC "PRIM_CHR" #-} P.chr


