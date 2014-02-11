
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -O #-}
module Smten.GHC.Classes (
    (&&), (||), not,
    Eq(..), Ord(..),
    ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Classes instead of Smten.Compiled.Smten.GHC.Classes

import GHC.Prim
import GHC.Types

infix 4 <, <=, >=, >
infix 4 ==, /=
infixr 3 &&
infixr 2 ||


-- Note: this has to match the definition from Prelude
class Eq a where
    (==) :: a -> a -> Bool
    (==) x y = not (x /= y)

    (/=) :: a -> a -> Bool
    (/=) x y = not (x == y)

instance Eq Int where
    (==) = eqInt
    (/=) = neInt

{-# INLINE eqInt #-}
{-# INLINE neInt #-}
eqInt, neInt :: Int -> Int -> Bool
(I# x) `eqInt` (I# y) = x ==# y
(I# x) `neInt` (I# y) = x /=# y

instance Eq () where
    (==) () () = True

instance (Eq a, Eq b) => Eq (a, b) where
    (==) (a, b) (c, d) = (a == c) && (b == d)

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
    (==) (a1, a2, a3) (b1, b2, b3) = (a1 == b1) && (a2 == b2) && (a3 == b3)

instance (Eq a) => Eq [a] where
    (==) [] [] = True
    (==) (a:as) (b:bs) = a == b && as == bs
    (==) _ _ = False

instance Eq Bool where
    (==) True True = True
    (==) True False = False
    (==) False True = False
    (==) False False = True

instance Eq Char where
    (C# c1) == (C# c2) = c1 `eqChar#` c2
    (C# c1) /= (C# c2) = c1 `neChar#` c2

instance Eq Ordering where
   (==) LT LT = True
   (==) EQ EQ = True
   (==) GT GT = True
   (==) _ _ = False

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y

instance Ord Int where
    (<)     = ltInt
    (<=)    = leInt
    (>=)    = geInt
    (>)     = gtInt

{-# INLINE gtInt #-}
{-# INLINE geInt #-}
{-# INLINE ltInt #-}
{-# INLINE leInt #-}
gtInt, geInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = x >#  y
(I# x) `geInt` (I# y) = x >=# y
(I# x) `ltInt` (I# y) = x <#  y
(I# x) `leInt` (I# y) = x <=# y


instance (Ord a, Ord b) => Ord (a, b) where
    (<=) (a, b) (c, d) = (a < c) || (a == c && b <= d)

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
    (<=) (a1, a2, a3) (b1, b2, b3) =
        (a1 < b1) || (a1 == b1 && (a2 < b2 || a2 == b2 && a3 <= b3))
    
instance (Ord a) => Ord [a] where
    (<=) [] [] = True
    (<=) [] (_:_) = True
    (<=) (_:_) [] = False
    (<=) (a:as) (b:bs) = a < b || (a == b && as <= bs)

instance Ord Char where
    (C# c1) >  (C# c2) = c1 `gtChar#` c2
    (C# c1) >= (C# c2) = c1 `geChar#` c2
    (C# c1) <= (C# c2) = c1 `leChar#` c2
    (C# c1) <  (C# c2) = c1 `ltChar#` c2

(&&) :: Bool -> Bool -> Bool
(&&) True x = x
(&&) False _ = False

(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) False x = x

not :: Bool -> Bool
not True = False
not False = True

