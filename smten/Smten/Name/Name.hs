
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-cse #-}

module Smten.Name.Name (
    Name, name, unname
    ) where

import Data.Hashable
import qualified Data.ByteString.Char8 as STR

import Smten.Ppr

newtype Name = Name {
    nm_str :: STR.ByteString
} deriving (Ord, Eq, Hashable)

name :: String -> Name
name = Name . STR.pack

unname :: Name -> String
unname = STR.unpack . nm_str

instance Show Name where
    show = show . nm_str

instance Ppr Name where
    ppr = text . unname

