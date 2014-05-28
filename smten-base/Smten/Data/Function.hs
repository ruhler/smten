
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.Data.Function (
    id, const, (.), flip, ($),
    fix, on,
    ) where

import Prelude

infixl 0 `on`

fix :: (a -> a) -> a
fix f = let x = f x in x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)

