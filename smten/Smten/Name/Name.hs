
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smten.Name.Name (
    Name, name, unname, nhead, ntail, nnull, nappend,
    ) where

import Data.Hashable
import qualified Data.ByteString.Char8 as STR

import Smten.Ppr

newtype Name = Name {
    nm_str :: STR.ByteString
} deriving (Eq, Ord, Hashable)

name :: String -> Name
name s = {-# SCC "name" #-} Name (STR.pack s)

unname :: Name -> String
unname = STR.unpack . nm_str

nhead :: Name -> Char
nhead = STR.head . nm_str

ntail :: Name -> Name
ntail = Name . STR.tail . nm_str

nnull :: Name -> Bool
nnull = STR.null . nm_str

nappend :: Name -> Name -> Name
nappend (Name a) (Name b) = Name (STR.append a b)

instance Show Name where
    show (Name x) = show x

instance Ppr Name where
    ppr = text . unname

