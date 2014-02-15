
module Smten.Compiled.Smten.Data.Num0 (
    int_fromInteger,
    ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf

int_fromInteger :: Integer -> Int
int_fromInteger = {-# SCC "PRIM_INT_FROMINTEGER" #-}  symapp P.fromInteger

