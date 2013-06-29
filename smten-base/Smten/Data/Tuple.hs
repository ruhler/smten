
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Tuple (
    fst, snd, curry, uncurry, swap
 ) where

import Smten.Data.Bool
import Smten.Data.Eq

instance (Eq a, Eq b) => Eq (a, b) where
    (==) (a, b) (x, y) = a == x && b == y

fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

