
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Show0 (
    integer_showsPrec,
 ) where

import qualified Prelude as P
import Smten.Runtime.SymbolicOf
import Smten.Compiled.Smten.Smten.Base

integer_showsPrec :: Int -> Integer -> List__ Char -> List__ Char
integer_showsPrec a b c = {-# SCC "PRIM_INTEGER_SHOWSPREC" #-} symapp2 (\av bv ->
    fromHSString (P.showsPrec av (bv :: P.Integer) (toHSString c))) a b

