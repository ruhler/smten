
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Show0 (
   integer_showsPrec
    ) where

import qualified Prelude as P
import Prelude(String, Char, Int, Integer)
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE integer_showsPrec #-}
integer_showsPrec :: Int -> Integer -> String -> String
integer_showsPrec = {-# SCC "PRIM_INTEGER_SHOWSPREC" #-} P.showsPrec

