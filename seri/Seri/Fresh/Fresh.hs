
module Seri.Fresh.Fresh (
    Fresh(..)
    ) where

import Seri.Sig

class (Monad m) => Fresh m where
    -- Return a fresh name based on the given name.
    fresh :: Sig -> m Sig
    
