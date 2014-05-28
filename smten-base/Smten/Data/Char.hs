
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char (
    Char, isSpace, ord, chr,
    digitToInt, isDigit, isUpper, isLower, isAlpha, isAlphaNum,
 ) where

import qualified Prelude as P
import Prelude(Char, Int, error)
import Smten.Data.Bool
import Smten.Data.Eq
import Smten.Data.Ord
import GHC.Base(ord)
import GHC.Char(chr)
import GHC.Num(Num(..))

isSpace :: Char -> Bool
isSpace c = c == ' '    
         || c == '\t'
         || c == '\n'
         || c == '\r'
         || c == '\f'
         || c == '\v'
         || c == '\xa0'

digitToInt :: Char -> Int
digitToInt c
 | isDigit c = ord c - ord '0'
 | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
 | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
 | otherwise = error "Char.digitToInt: not a digit"

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isUpper :: Char -> Bool
isUpper c =  c >= 'A' && c <= 'Z'
          || c >= '\xC0' && c <= '\xD6' 
          || c >= '\xD8' && c <= '\xDE'

isLower :: Char -> Bool
isLower c =  c >= 'a' && c <= 'z'   
          || c >= '\xDF' && c <= '\xF6'
          || c >= '\xF8' && c <= '\xFF'

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

