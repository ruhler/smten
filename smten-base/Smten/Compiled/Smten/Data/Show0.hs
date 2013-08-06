
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Show0 (
    int_showsPrec, integer_showsPrec, char_showsPrec, char_showList,
 ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf

-- TODO: These convert between P.String and List__ Char all over the place.
-- Surely something more efficient can be done?
int_showsPrec :: Int -> Int -> List__ Char -> List__ Char
int_showsPrec a b c = symapp2 (\av bv ->
    fromHSString (P.showsPrec av (bv :: P.Int) (toHSString c))) a b

integer_showsPrec :: Int -> Integer -> List__ Char -> List__ Char
integer_showsPrec a b c = symapp2 (\av bv ->
    fromHSString (P.showsPrec av (bv :: P.Integer) (toHSString c))) a b

char_showsPrec :: Int -> Char -> List__ Char -> List__ Char
char_showsPrec a b c = symapp (\av ->
    fromHSString (P.showsPrec av (toHSChar b) (toHSString c))) a

char_showList :: List__ Char -> List__ Char -> List__ Char
char_showList a b = fromHSString (P.showList (toHSString a) (toHSString b))

