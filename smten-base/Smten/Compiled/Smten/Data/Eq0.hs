
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Eq0 (integer_eq) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool0
import Smten.Runtime.Formula

integer_eq :: Integer -> Integer -> Bool
integer_eq = {-# SCC "PRIM_INTEGER_EQ" #-} eq_IntegerF

