
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Char0 (char_eq) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

char_eq :: Char -> Char -> Bool
char_eq = symapp2 P.$ \av bv ->
    if P.asTypeOf av 'c' P.== bv
        then True
        else False

