
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Char0 (char_eq, ord) where

import qualified Prelude as P
import qualified Data.Char as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool0
import Smten.Runtime.SymbolicOf

char_eq :: Char -> Char -> Bool
char_eq = symapp2 P.$ \av bv ->
    if P.asTypeOf av 'c' P.== bv
        then True
        else False

ord :: Char -> Int
ord = symapp (tosym P.. P.ord)

