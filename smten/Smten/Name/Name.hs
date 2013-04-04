
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Name.Name (
    Name, name, unname, ncons, ntake, nhead, ntail, nnull, nappend,
    ) where

import Data.Monoid
import qualified Data.ByteString.Char8 as STR

import Smten.Ppr

type Name = STR.ByteString

name :: String -> Name
name = STR.pack

unname :: Name -> String
unname = STR.unpack

ncons :: Char -> Name -> Name
ncons = STR.cons

nhead :: Name -> Char
nhead = STR.head

ntail :: Name -> Name
ntail = STR.tail

ntake :: Int -> Name -> Name
ntake = STR.take

nnull :: Name -> Bool
nnull = STR.null

nappend :: Name -> Name -> Name
nappend = STR.append

instance Ppr Name where
    ppr = text . unname

