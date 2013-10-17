
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Read (
    ReadS, Read(..), reads, read,
   ) where

import Smten.Smten.Base
import Smten.Data.Char
import Smten.Data.Function
import Smten.Data.List
import Smten.Data.Num

type ReadS a = String -> [(a, String)]

class Read a where
    readsPrec :: Int -> ReadS a

reads :: (Read a) => ReadS a
reads = readsPrec 0

read :: (Read a) => String -> a
read s = case [x | (x, t) <- reads s, ("","") <- lex t] of
            [x] -> x
            [] -> error "Prelude.read: no parse"
            _ -> error "Prelude.read: ambiguous parse"

lex :: ReadS String
lex "" = [("","")]
lex (c:s) | isSpace c = lex (dropWhile isSpace s)
lex x = error $ "Prelude.lex: TODO: lex " ++ x

readDec :: (Num n) => String -> (n, String)
readDec ('-':xs) =
  case readDec xs of
     (n, s) -> (negate n, s)
readDec xs = readDec' 0 xs
  where 
    readDec' x ('0':xs) = readDec' (x*10 + 0) xs
    readDec' x ('1':xs) = readDec' (x*10 + 1) xs
    readDec' x ('2':xs) = readDec' (x*10 + 2) xs
    readDec' x ('3':xs) = readDec' (x*10 + 3) xs
    readDec' x ('4':xs) = readDec' (x*10 + 4) xs
    readDec' x ('5':xs) = readDec' (x*10 + 5) xs
    readDec' x ('6':xs) = readDec' (x*10 + 6) xs
    readDec' x ('7':xs) = readDec' (x*10 + 7) xs
    readDec' x ('8':xs) = readDec' (x*10 + 8) xs
    readDec' x ('9':xs) = readDec' (x*10 + 9) xs
    readDec' x s = (x, s)

instance Read Integer where
    readsPrec _ x = [readDec x]

instance Read Int where
    readsPrec _ x = [readDec x]

