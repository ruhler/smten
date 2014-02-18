
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.Data.Function (
    id, const, (.), flip, ($),
    on,
    ) where

import GHC.Base(id, const, (.), flip, ($))

infixl 0 `on`

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)


