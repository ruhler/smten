
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Char0 (char_eq, char_leq, ord, chr) where

import qualified Prelude as P
import qualified Data.Char as P
import GHC.Prim
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool0
import Smten.Runtime.SymbolicOf

char_eq :: Char -> Char -> Bool
char_eq (C# a) (C# b) = {-# SCC "PRIM_CHAR_EQ" #-}
    if (eqChar# a b) then __True else __False
char_eq a b = {-# SCC "PRIM_CHAR_EQ" #-} (symapp2 P.$ \av bv ->
    if P.asTypeOf av 'c' P.== bv
        then __True
        else __False) a b

char_leq :: Char -> Char -> Bool
char_leq (C# a) (C# b) = {-# SCC "PRIM_CHAR_LEQ" #-}
    if (leChar# a b) then __True else __False
char_leq a b = {-# SCC "PRIM_CHAR_LEQ" #-} (symapp2 P.$ \av bv ->
    if P.asTypeOf av 'c' P.<= bv
        then __True
        else __False) a b

ord :: Char -> Int
ord = {-# SCC "PRIM_ORD" #-} symapp (tosym P.. P.ord)

chr :: Int -> Char
chr = {-# SCC "PRIM_CHR" #-} symapp (tosym P.. P.chr)

