
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.Data.Function (
    id, const, (.), flip, ($), -- fix,
    on,
    ) where

infixr 9 .
infixl 0 `on`
infixr 0 $

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
($) f x = f x

--fix :: (a -> a) -> a
--fix f = let x = f x in x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)


