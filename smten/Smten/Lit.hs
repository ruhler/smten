
{-# LANGUAGE PatternGuards #-}

module Smten.Lit (
    Lit(),
    integerL, de_integerL,
    charL, de_charL,
    bitL, de_bitL,
    dynamicL, de_dynamicL,
    ) where

import Data.Typeable
import Data.Dynamic

import Smten.Bit
import Smten.Ppr

data Lit = DynamicL Dynamic
         | IntegerL Integer
         | CharL Char
         | BitL Bit
    deriving (Show)

instance Eq Lit where
    (==) (IntegerL a) (IntegerL b) = a == b
    (==) (CharL a) (CharL b) = a == b
    (==) (BitL a) (BitL b) = a == b
    (==) _ _ = False

dynamicL :: (Typeable a) => a -> Lit
dynamicL = {-# SCC "dynamicL" #-} DynamicL . toDyn

de_dynamicL :: (Typeable a) => Lit -> Maybe a
de_dynamicL (DynamicL x) = {-# SCC "de_dynamicL" #-} fromDynamic x
de_dynamicL _ = Nothing

integerL :: Integer -> Lit
integerL = {-# SCC "integerL" #-} IntegerL

de_integerL :: Lit -> Maybe Integer
de_integerL (IntegerL i) = {-# SCC "de_integerL" #-} Just i
de_integerL _ = Nothing

charL :: Char -> Lit
charL = {-# SCC "charL" #-} CharL

de_charL :: Lit -> Maybe Char
de_charL (CharL c) = {-# SCC "de_charL" #-} Just c
de_charL _ = Nothing

bitL :: Bit -> Lit
bitL = {-# SCC "bitL" #-} BitL

de_bitL :: Lit -> Maybe Bit
de_bitL (BitL b) = {-# SCC "de_bitL" #-} Just b
de_bitL _ = Nothing

instance Ppr Lit where
    ppr (IntegerL i) = integer i
    ppr (CharL c) = text (show c)
    ppr (BitL b) = text (show b)
    ppr _ = text "?Lit?"

