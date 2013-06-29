
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Char (
    Char, isSpace
 ) where

import qualified Prelude as P
import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Eq

instance Eq Char where 
   (==) a b = if a P.== b then True else False

isSpace :: Char -> Bool
isSpace c = c == ' '    
         || c == '\t'
         || c == '\n'
         || c == '\r'
         || c == '\f'
         || c == '\v'
         || c == '\xa0'
