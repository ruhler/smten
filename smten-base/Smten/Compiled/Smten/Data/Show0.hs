
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Show0 (
    int_showsPrec, integer_showsPrec, char_showsPrec, char_showList,
 ) where

import qualified Prelude as P
import Smten.Runtime.SymbolicOf
import Smten.Compiled.Smten.Smten.Base

-- TODO: These convert between P.String and List__ Char all over the place.
-- Surely something more efficient can be done?
int_showsPrec :: Int -> Int -> List__ Char -> List__ Char
int_showsPrec a b c = {-# SCC "PRIM_INT_SHOWSPREC" #-} symapp2 (\av bv ->
    fromHSString (P.showsPrec av (bv :: P.Int) (toHSString c))) a b

integer_showsPrec :: Int -> Integer -> List__ Char -> List__ Char
integer_showsPrec a b c = {-# SCC "PRIM_INTEGER_SHOWSPREC" #-} symapp2 (\av bv ->
    fromHSString (P.showsPrec av (bv :: P.Integer) (toHSString c))) a b

char_showsPrec :: Int -> Char -> List__ Char -> List__ Char
char_showsPrec a b c = {-# SCC "PRIM_CHAR_SHOWSPREC" #-} symapp (\av ->
    fromHSString (P.showsPrec av (toHSChar b) (toHSString c))) a

char_showList :: List__ Char -> List__ Char -> List__ Char
char_showList a b = {-# SCC "PRIM_CHAR_SHOWLIST" #-} fromHSString (P.showList (toHSString a) (toHSString b))

