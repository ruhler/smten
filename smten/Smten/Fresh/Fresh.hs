
module Smten.Fresh.Fresh (
    Fresh(..)
    ) where

import Smten.Sig

class (Functor m, Monad m) => Fresh m where
    -- Return a fresh name based on the given name.
    fresh :: Sig -> m Sig
    
