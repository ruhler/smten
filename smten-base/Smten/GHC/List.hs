
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.List (
    map, (++), filter, concat,
    head, last, tail, init, null, length, (!!),
    foldl, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    reverse, and, or,
    any, all, elem, notElem, lookup,
    concatMap,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    errorEmptyList,
     ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.List instead of Smten.Compiled.Smten.GHC.List
import Data.Maybe
import GHC.Base
import GHC.Num

infixl 9 !!
infix 4 `elem`, `notElem`

head                    :: [a] -> a
head (x:_)              =  x
head []                 =  badHead

badHead :: a
badHead = errorEmptyList "head"

tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  errorEmptyList "tail"

last :: [a] -> a
last []                 =  errorEmptyList "last"
last (x:xs)             =  last' x xs
  where last' y []     = y
        last' _ (y:ys) = last' y ys

init :: [a] -> [a]
init []                 =  errorEmptyList "init"
init (x:xs)             =  init' x xs
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs

null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False

length                  :: [a] -> Int
length []               =  0
length (x:xs) = 1 + length xs

filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
 
foldl        :: (a -> b -> a) -> a -> [b] -> a
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs

scanl                   :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls            =  q : (case ls of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  []

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  errorEmptyList "foldr1"

scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []             =  []
scanr1 _ [x]            =  [x]
scanr1 f (x:xs)         =  f x q : qs
                           where qs@(q:_) = scanr1 f xs

iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)
 
repeat :: a -> [a]
{-# INLINE [0] repeat #-}
repeat x = xs where xs = x : xs

replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

cycle                   :: [a] -> [a]
cycle []                = error "Prelude.cycle: empty list"
cycle xs                = xs' where xs' = xs ++ xs'

takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs


take                   :: Int -> [a] -> [a]
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs

drop                   :: Int -> [a] -> [a]
drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs

splitAt                :: Int -> [a] -> ([a],[a])
splitAt n xs           =  (take n xs, drop n xs)

span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

break                   :: (a -> Bool) -> [a] -> ([a],[a])
break _ xs@[]           =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)

reverse                 :: [a] -> [a]
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)

and                     :: [Bool] -> Bool
or                      :: [Bool] -> Bool
and []          =  True
and (x:xs)      =  x && and xs
or []           =  False
or (x:xs)       =  x || or xs


any                     :: (a -> Bool) -> [a] -> Bool
any _ []        = False
any p (x:xs)    = p x || any p xs

all                     :: (a -> Bool) -> [a] -> Bool
all _ []        =  True
all p (x:xs)    =  p x && all p xs

elem                    :: (Eq a) => a -> [a] -> Bool
elem _ []       = False
elem x (y:ys)   = x==y || elem x ys

notElem                 :: (Eq a) => a -> [a] -> Bool
notElem _ []    =  True
notElem x (y:ys)=  x /= y && notElem x ys

lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []

concat :: [[a]] -> [a]
concat = foldr (++) []

(!!)                    :: [a] -> Int -> a
xs     !! n | n < 0 =  error "Prelude.!!: negative index"
[]     !! _         =  error "Prelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)

zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []

zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                        =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _        =  []

unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

unzip3   :: [(a,b,c)] -> ([a],[b],[c])
{-# INLINE unzip3 #-}
unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                  ([],[],[])


errorEmptyList :: String -> a
errorEmptyList fun =
  error (prel_list_str ++ fun ++ ": empty list")

prel_list_str :: String
prel_list_str = "Prelude."

