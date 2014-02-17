
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.Data.Function (
    id, const, (.), flip, ($), -- fix,
    on,
    ) where

import GHC.Base(const, (.))

infixl 0 `on`
infixr 0 $

id :: a -> a
id x = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
($) f x = f x

--fix :: (a -> a) -> a
--fix f = let x = f x in x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)


