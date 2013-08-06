
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.List0 (
    map, (++), foldr,
 ) where

infixr 5 ++

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (x:xs) b = x : (xs ++ b)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

