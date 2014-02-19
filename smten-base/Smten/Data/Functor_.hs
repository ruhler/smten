
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Functor_ (
    Functor(fmap), (<$), (<$>),
    ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.Data.Functor instead of Smten.Compiled.Smten.Data.Functor_

import GHC.Base(Functor(..))
infixl 4 <$>

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

