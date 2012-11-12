
module Seri.Lit (Lit(..)) where

data Lit = IntegerL Integer         -- ^ integer literal
         | CharL Char               -- ^ character literal
    deriving (Eq, Ord, Show)

