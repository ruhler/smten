
module Seri.Sig (Sig(..)) where

import Seri.Name
import Seri.Type
import Seri.Ppr

-- | 'Sig' is a name annotated with a type.
data Sig = Sig Name Type
    deriving(Eq, Ord, Show)

instance Typeof Sig where
    typeof (Sig _ t) = t

instance Ppr Sig where
    ppr (Sig n UnknownT) = ppr n
    ppr (Sig n t) = parens (ppr n <+> text "::" <+> ppr t)

