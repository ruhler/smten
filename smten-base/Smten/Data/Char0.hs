
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char0 (ord, chr) where

import qualified Data.Char as P
import qualified Prelude as P
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE ord #-}
ord :: P.Char -> P.Int
ord = {-# SCC "PRIM_ORD" #-} P.ord

{-# NOINLINE chr #-}
chr :: P.Int -> P.Char
chr = {-# SCC "PRIM_CHR" #-} P.chr


