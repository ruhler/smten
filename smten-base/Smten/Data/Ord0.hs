
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.Ord0 (
    Ord(..),
    Ordering(..),
    comparing,
 ) where

import Smten.Data.Bool
import Smten.Data.Eq

infix 4 <, <=, >=, >

class (Eq a) => Ord a where
    compare :: a -> a -> Ordering
    compare x y
       | x == y = EQ
       | x <= y = LT
       | otherwise = GT

    (<) :: a -> a -> Bool
    (<) x y = compare x y == LT

    (<=) :: a -> a -> Bool
    (<=) x y = compare x y /= GT

    (>=) :: a -> a -> Bool
    (>=) x y = compare x y /= LT

    (>) :: a -> a -> Bool
    (>) x y = compare x y == GT

    max :: a -> a -> a
    max x y 
      | x <= y = y
      | otherwise = x

    min :: a -> a -> a
    min x y
      | x <= y = x
      | otherwise = y

data Ordering = LT | EQ | GT

instance Eq Ordering where
    (==) LT LT = True
    (==) EQ EQ = True
    (==) GT GT = True
    (==) _ _ = False

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

