
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.List0 (
    map, (++), foldr, build,
 ) where

import GHC.Base (foldr, build)
infixr 5 ++

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
(++) [] b = b
(++) (x:xs) b = x : (xs ++ b)

