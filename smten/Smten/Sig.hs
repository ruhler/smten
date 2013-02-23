
module Smten.Sig (Sig(..)) where

import Smten.Name
import Smten.Type
import Smten.Ppr

-- | 'Sig' is a name annotated with a type.
data Sig = Sig Name Type
    deriving(Eq, Ord, Show)

instance Typeof Sig where
    typeof (Sig _ t) = t

instance Ppr Sig where
    ppr (Sig n UnknownT) = ppr n
    ppr (Sig n t) = parens (ppr n <+> text "::" <+> ppr t)

instance Assign Sig where
    assignl f (Sig n t) = Sig n (assignl f t) 

