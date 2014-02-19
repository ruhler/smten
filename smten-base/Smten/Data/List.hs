
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.List (
    map, (++), filter, concat, concatMap,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,

    sort, sortBy, tails, isPrefixOf, isInfixOf, nub,
 ) where

import GHC.Classes (Ord(..))
import GHC.List 
import GHC.Num(Num(..))
import GHC.Types(Ordering(..))
import Smten.Smten.Base
import Smten.Data.Bool
import qualified Smten.Data.Char as Char
import Smten.Data.Eq
import Smten.Data.Function

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ [] = error "Prelude.foldl1: empty list"


lines :: String -> [String]
lines "" = []
lines s = let (l, s') = break (== '\n') s
          in l : case s' of
                   [] -> []
                   (_:s'') -> lines s''

words :: String -> [String]
words s = case dropWhile Char.isSpace s of
            "" -> []
            s' -> w : words s''
                  where (w, s'') = break Char.isSpace s'

unlines :: [String] -> String
unlines = concatMap (++ "\n")

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ':s) ws

sum :: (Num a) => [a] -> a
sum = foldl (+) 0

product :: (Num a) => [a] -> a
product = foldl (*) 1

maximum :: (Ord a) => [a] -> a
maximum [] = error "Prelude.maximum: empty list"
maximum xs = foldl1 max xs

minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

sort :: (Ord a) => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
   where
     sequences (a:b:xs)
       | (a `cmp` b) == GT = descending b [a]  xs
       | otherwise       = ascending  b ((:) a) xs
     sequences xs = [xs]

     descending a as (b:bs)
       | (a `cmp` b) == GT = descending b (a:as) bs
     descending a as bs  = (a:as): sequences bs

     ascending a as (b:bs)
       | (a `cmp` b) /= GT = ascending b (\ys -> as (a:ys)) bs
     ascending a as bs   = as [a]: sequences bs

     mergeAll [x] = x
     mergeAll xs  = mergeAll (mergePairs xs)

     mergePairs (a:b:xs) = merge a b: mergePairs xs
     mergePairs xs       = xs

     merge as@(a:as') bs@(b:bs')
       | (a `cmp` b) == GT = b:merge as  bs'
       | otherwise       = a:merge as' bs
     merge [] bs         = bs
     merge as []         = as

tails :: [a] -> [[a]]
tails x = x : (if null x then [] else tails (tail x))

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
isPrefixOf _ _ = False

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter ((/=) x) xs)

