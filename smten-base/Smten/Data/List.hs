
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

import GHC.Base (foldr, map, (++))
import GHC.List (head, tail, last, init, null, filter, foldl, scanl, scanl1)
import GHC.List (foldr1, scanr, scanr1, iterate, repeat, replicate, cycle)
import GHC.List (takeWhile, dropWhile, take, drop, splitAt, span, break, reverse)
import GHC.List (and, or, any, all, elem, notElem, lookup, concatMap, concat)
import GHC.Num(Num(..))
import Smten.Smten.Base
import Smten.Data.Bool
import qualified Smten.Data.Char as Char
import Smten.Data.Eq
import Smten.Data.Function

infixl 9 !!

length :: [a] -> Int
length [] = 0
length (_:l) = 1 + length l

(!!) :: [a] -> Int -> a
(!!) xs n | n < 0 = error "Prelude.!!: negative index"
(!!) [] _ = error "Prelude.!!: index too large"
(!!) (x:_) n | n == 0 = x
(!!) (_:xs) n = xs !! (n-1)


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

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z (a:as) (b:bs) = z a b : zipWith z as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a:as) (b:bs) (c:cs) = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ = []

unzip :: [(a,b)] -> ([a],[b])
unzip = foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([], [])

unzip3 :: [(a,b,c)] -> ([a],[b],[c])
unzip3 = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs)) ([],[],[])

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

