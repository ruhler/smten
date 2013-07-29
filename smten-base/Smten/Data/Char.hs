
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char (
    Char, isSpace, eqString, ord, chr,
 ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Char0
import Smten.Data.Eq

instance Eq Char where 
   (==) = char_eq

-- used for string literal pattern matching.
eqString :: String -> String -> Bool
eqString [] [] = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString _ _ = False

isSpace :: Char -> Bool
isSpace c = c == ' '    
         || c == '\t'
         || c == '\n'
         || c == '\r'
         || c == '\f'
         || c == '\v'
         || c == '\xa0'
