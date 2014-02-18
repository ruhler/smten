
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
import Smten.Smten.Base
import Smten.Data.Bool
import qualified Smten.Data.Char as Char
import Smten.Data.Eq
import Smten.Data.Function
import Smten.Data.Maybe
import Smten.Data.Num
import Smten.Data.Ord

infixl 9 !!
infix 4 `elem`, `notElem`

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x = x : filter p xs
                | otherwise = filter p xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head :: [a] -> a
head (x:_) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_:xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs
init [] = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null (_:_) = False

length :: [a] -> Int
length [] = 0
length (_:l) = 1 + length l

(!!) :: [a] -> Int -> a
(!!) xs n | n < 0 = error "Prelude.!!: negative index"
(!!) [] _ = error "Prelude.!!: index too large"
(!!) (x:_) n | n == 0 = x
(!!) (_:xs) n = xs !! (n-1)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ [] = error "Prelude.foldl1: empty list"

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs = q : (case xs of
                      [] -> []  
                      x:xs -> scanl f (f q x) xs)

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs) = scanl f x xs
scanl1 _ [] = []

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 _ [] = error "Prelude.foldr1: empty list"

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 [] = [q0]
scanr f q0 (x:xs) = f x q : qs
                    where qs@(q:_) = scanr f q0 xs

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f [] = []
scanr1 f [x] = [x]
scanr1 f (x:xs) = f x q : qs
                  where qs@(q:_) = scanr1 f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x:xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
   | p x = x : takeWhile p xs
   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p xs@(x:xs')
  | p x = dropWhile p xs'
  | otherwise = xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])
span p xs@(x:xs')
 | p x = (x:ys, zs)
 | otherwise = ([], xs)
                       where (ys, zs) = span p xs'

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

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

reverse :: [a] -> [a]
reverse l = rev l []
  where rev [] a = a
        rev (x:xs) a = rev xs (x:a)

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

elem :: (Eq a) => a -> [a] -> Bool
elem x = any (== x)

notElem :: (Eq a) => a -> [a] -> Bool
notElem x = all (/= x)

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x,y):xys)
 | key == x = Just y
 | otherwise = lookup key xys

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

