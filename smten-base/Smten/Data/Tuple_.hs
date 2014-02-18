
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Tuple_ (
    fst, snd, curry, uncurry, swap
 ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.Data.Tuple instead of Smten.Compiled.Smten.Data.Tuple_

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

