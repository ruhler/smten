
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Functor (
    Functor(fmap), (<$), (<$>),
    ) where

import GHC.Base(Functor(..))
infixl 4 <$>

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

