
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Char0 (ord, chr) where

import qualified Prelude as P
import qualified Data.Char as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf

ord :: Char -> Int
ord = {-# SCC "PRIM_ORD" #-} symapp (tosym P.. P.ord)

chr :: Int -> Char
chr = {-# SCC "PRIM_CHR" #-} symapp (tosym P.. P.chr)

