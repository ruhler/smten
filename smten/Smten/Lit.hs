
{-# LANGUAGE PatternGuards #-}

module Smten.Lit (
    Lit(..),
    integerL, de_integerL,
    charL, de_charL,
    ) where

import Smten.Ppr

data Lit = IntegerL Integer
         | CharL Char
    deriving (Show)

instance Eq Lit where
    (==) (IntegerL a) (IntegerL b) = a == b
    (==) (CharL a) (CharL b) = a == b

integerL :: Integer -> Lit
integerL = IntegerL

de_integerL :: Lit -> Maybe Integer
de_integerL (IntegerL i) = Just i
de_integerL _ = Nothing

charL :: Char -> Lit
charL = CharL

de_charL :: Lit -> Maybe Char
de_charL (CharL c) = Just c
de_charL _ = Nothing

instance Ppr Lit where
    ppr (IntegerL i) = integer i
    ppr (CharL c) = text (show c)

