
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.List_ (
    (++), head, last, tail, init, null, length,
    map, reverse,
    foldl, foldl1, foldr, foldr1,
    concat, concatMap, and, or, any, all,
    sum, product, maximum, minimum,
    scanl, scanl1,  scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    tails,
    isPrefixOf, isInfixOf,
    elem, notElem, lookup,
    filter,
    (!!),
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    lines, words, unlines, unwords,
    nub,
    sort, sortBy,
 ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.Data.List instead of Smten.Compiled.Smten.Data.List_

import GHC.Num
import GHC.List 

import GHC.Classes (Ord(..))
import GHC.Types(Ordering(..))
import Smten.Smten.Base
import Smten.Data.Bool
import qualified Smten.Data.Char as Char
import Smten.Data.Eq
import Smten.Data.Function

isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

nub :: (Eq a) => [a] -> [a]
nub l                   = nub' l []
  where
    nub' [] _           = []
    nub' (x:xs) ls
        | x `elem` ls   = nub' xs ls
        | otherwise     = x : nub' xs (x:ls)

maximum :: (Ord a) => [a] -> a
maximum [] = errorEmptyList "maximum"
maximum xs = foldl1 max xs

minimum                 :: (Ord a) => [a] -> a
minimum []              =  errorEmptyList "minimum"
minimum xs              =  foldl1 min xs


tails                   :: [a] -> [[a]]
tails xs                =  xs : case xs of
                                  []      -> []
                                  _ : xs' -> tails xs'


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

foldl1                  :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)         =  foldl f x xs
foldl1 _ []             =  errorEmptyList "foldl1"

{-# SPECIALISE sum     :: [Int] -> Int #-}
{-# SPECIALISE sum     :: [Integer] -> Integer #-}
{-# SPECIALISE product :: [Int] -> Int #-}
{-# SPECIALISE product :: [Integer] -> Integer #-}
sum :: (Num a) => [a] -> a
sum     l       = sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a+x)

product :: (Num a) => [a] -> a
product l       = prod l 1
  where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)
 


lines :: String -> [String]
lines "" = []
lines s = let (l, s') = break (== '\n') s
          in l : case s' of
                   [] -> []
                   (_:s'') -> lines s''

unlines :: [String] -> String
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls

words :: String -> [String]
words s = case dropWhile Char.isSpace s of
            "" -> []
            s' -> w : words s''
                  where (w, s'') = break Char.isSpace s'

unwords :: [String] -> String
unwords []              =  ""
unwords [w]             = w
unwords (w:ws)          = w ++ ' ' : unwords ws

