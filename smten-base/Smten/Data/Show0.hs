
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Show0 (
   char_showsPrec, char_showList, integer_showsPrec
    ) where

import qualified Prelude as P
import Prelude(String, Char, Int, Integer)
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE integer_showsPrec #-}
integer_showsPrec :: Int -> Integer -> String -> String
integer_showsPrec = {-# SCC "PRIM_INTEGER_SHOWSPREC" #-} P.showsPrec

{-# NOINLINE char_showsPrec #-}
char_showsPrec :: Int -> Char -> String -> String
char_showsPrec = {-# SCC "PRIM_CHAR_SHOWSPREC" #-} P.showsPrec

{-# NOINLINE char_showList #-}
char_showList :: [Char] -> String -> String
char_showList = {-# SCC "PRIM_CHAR_SHOWLIST" #-} P.showList

