
module Seri.Sig (Sig(..)) where

import Seri.Name
import Seri.Type.Type

-- | 'Sig' is a name annotated with a type.
data Sig = Sig Name Type
    deriving(Eq, Ord, Show)

