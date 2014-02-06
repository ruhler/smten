
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Functor (
    Functor(..), (<$>),
    ) where

import Smten.Data.Function

infixl 4 <$>
infixl 4 <$

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    (<$) :: a -> f b -> f a
    (<$) = fmap . const

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

