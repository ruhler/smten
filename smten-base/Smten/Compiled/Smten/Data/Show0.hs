
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Show0 (
    int_showsPrec, integer_showsPrec, char_showsPrec, char_showList,
 ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base

-- TODO: These convert between P.String and List__ Char all over the place.
-- Surely something more efficient can be done?
int_showsPrec :: Int -> Int -> List__ Char -> List__ Char
int_showsPrec a b c = toList__ (P.showsPrec a b (fromList__ c))

integer_showsPrec :: Int -> Integer -> List__ Char -> List__ Char
integer_showsPrec a (Integer b) c = toList__ (P.showsPrec a b (fromList__ c))

char_showsPrec :: Int -> Char -> List__ Char -> List__ Char
char_showsPrec a b c = toList__ (P.showsPrec a b (fromList__ c))

char_showList :: List__ Char -> List__ Char -> List__ Char
char_showList a b = toList__ (P.showList (fromList__ a) (fromList__ b))

