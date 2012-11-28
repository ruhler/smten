
module Seri.Lit (
    Lit(),
    integerL, de_integerL,
    charL, de_charL,
    ) where

import Seri.Ppr

data Lit = IntegerL Integer         -- ^ integer literal
         | CharL Char               -- ^ character literal
    deriving (Eq, Ord, Show)

integerL :: Integer -> Lit
integerL = IntegerL

de_integerL :: Lit -> Maybe Integer
de_integerL (IntegerL x) = Just x
de_integerL _ = Nothing

charL :: Char -> Lit
charL = CharL

de_charL :: Lit -> Maybe Char
de_charL (CharL x) = Just x
de_charL _ = Nothing

instance Ppr Lit where
    ppr (IntegerL i) = integer i
    ppr (CharL c) = text (show c)


