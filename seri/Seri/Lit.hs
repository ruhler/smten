
module Seri.Lit (Lit(..)) where

import Seri.Ppr

data Lit = IntegerL Integer         -- ^ integer literal
         | CharL Char               -- ^ character literal
    deriving (Eq, Ord, Show)

instance Ppr Lit where
    ppr (IntegerL i) = integer i
    ppr (CharL c) = text (show c)

