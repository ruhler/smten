
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Either (
    Either(..), either,
    ) where

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

