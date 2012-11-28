
module Seri.HaskellF.Symbolic (
    Symbolic(..),
    ) where

import Seri.Type
import Seri.ExpH

-- | Symbolic represents new type wrappers around ExpH.
class (SeriT a) => Symbolic a where
    box :: ExpH -> a
    unbox :: a -> ExpH

-- | Convert a concrete haskell value to its symbolic representation.
seriS :: (SeriEH c, Symbolic f) => c -> f
seriS x = box . seriEH

de_seriS :: (Symbolic f, SeriEH c) => f -> Maybe c
de_seriS x = de_seriEH . unbox

