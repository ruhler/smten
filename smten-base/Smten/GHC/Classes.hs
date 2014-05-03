
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -O #-}
module Smten.GHC.Classes (
    (&&), (||), not,
    Eq(..), Ord(..),
    ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Classes instead of Smten.Compiled.Smten.GHC.Classes
--
-- This is intended to match GHC/Classes.hs as best as possible.

import qualified GHC.Classes as P
import GHC.Prim
import GHC.Types
import Smten.Smten.Unit
import Smten.Smten.Tuple

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||

-- | The 'Eq' class defines equality ('==') and inequality ('/=').
-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
-- and 'Eq' may be derived for any datatype whose constituents are also
-- instances of 'Eq'.
--
-- Minimal complete definition: either '==' or '/='.
--
class  Eq a  where
    (==), (/=)           :: a -> a -> Bool

    {-# INLINE (/=) #-}
    {-# INLINE (==) #-}
    x /= y               = not (x == y)
    x == y               = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}

deriving instance P.Eq Unit__
deriving instance (P.Eq a, P.Eq b) => P.Eq (Tuple2__ a b)
deriving instance (P.Eq a, P.Eq b, P.Eq c) => P.Eq (Tuple3__ a b c)
deriving instance (P.Eq a, P.Eq b, P.Eq c, P.Eq d) => P.Eq (Tuple4__ a b c d)

instance (Eq a) => Eq [a] where
    {-# SPECIALISE instance Eq [Char] #-}
    []     == []     = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _xs    == _ys    = False

-- TODO: can we auto-derive this?
instance Eq Bool where
    (==) True True = True
    (==) True False = False
    (==) False True = False
    (==) False False = True

-- TODO: can we auto-derive this?
instance Eq Ordering where
   (==) LT LT = True
   (==) EQ EQ = True
   (==) GT GT = True
   (==) _ _ = False

instance Eq Char where
    (C# c1) == (C# c2) = isTrue# (c1 `eqChar#` c2)
    (C# c1) /= (C# c2) = isTrue# (c1 `neChar#` c2)

instance Eq Int where
    (==) = eqInt
    (/=) = neInt

{-# INLINE eqInt #-}
{-# INLINE neInt #-}
eqInt, neInt :: Int -> Int -> Bool
(I# x) `eqInt` (I# y) = isTrue# (x ==# y)
(I# x) `neInt` (I# y) = isTrue# (x /=# y)

-- | The 'Ord' class is used for totally ordered datatypes.
--
-- Instances of 'Ord' can be derived for any user-defined
-- datatype whose constituent types are in 'Ord'.  The declared order
-- of the constructors in the data declaration determines the ordering
-- in derived 'Ord' instances.  The 'Ordering' datatype allows a single
-- comparison to determine the precise ordering of two objects.
--
-- Minimal complete definition: either 'compare' or '<='.
-- Using 'compare' can be more efficient for complex types.
--
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
    {-# MINIMAL compare | (<=) #-}

deriving instance P.Ord Unit__
deriving instance (P.Ord a, P.Ord b) => P.Ord (Tuple2__ a b)
deriving instance (P.Ord a, P.Ord b, P.Ord c) => P.Ord (Tuple3__ a b c)
deriving instance (P.Ord a, P.Ord b, P.Ord c, P.Ord d) => P.Ord (Tuple4__ a b c d)

instance (Ord a) => Ord [a] where
    {-# SPECIALISE instance Ord [Char] #-}
    compare []     []     = EQ
    compare []     (_:_)  = LT
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = case compare x y of
                                EQ    -> compare xs ys
                                other -> other

-- We don't use deriving for Ord Char, because for Ord the derived
-- instance defines only compare, which takes two primops.  Then
-- '>' uses compare, and therefore takes two primops instead of one.
instance Ord Char where
    (C# c1) >  (C# c2) = isTrue# (c1 `gtChar#` c2)
    (C# c1) >= (C# c2) = isTrue# (c1 `geChar#` c2)
    (C# c1) <= (C# c2) = isTrue# (c1 `leChar#` c2)
    (C# c1) <  (C# c2) = isTrue# (c1 `ltChar#` c2)
instance Ord Int where
    compare = compareInt
    (<)     = ltInt
    (<=)    = leInt
    (>=)    = geInt
    (>)     = gtInt

{-# INLINE gtInt #-}
{-# INLINE geInt #-}
{-# INLINE ltInt #-}
{-# INLINE leInt #-}
gtInt, geInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = isTrue# (x >#  y)
(I# x) `geInt` (I# y) = isTrue# (x >=# y)
(I# x) `ltInt` (I# y) = isTrue# (x <#  y)
(I# x) `leInt` (I# y) = isTrue# (x <=# y)

compareInt :: Int -> Int -> Ordering
(I# x#) `compareInt` (I# y#) = compareInt# x# y#

compareInt# :: Int# -> Int# -> Ordering
compareInt# x# y#
    | isTrue# (x# <#  y#) = LT
    | isTrue# (x# ==# y#) = EQ
    | True                = GT

-- OK, so they're technically not part of a class...:

-- Boolean functions

-- | Boolean \"and\"
(&&)                    :: Bool -> Bool -> Bool
True  && x              =  x
False && _              =  False

-- | Boolean \"or\"
(||)                    :: Bool -> Bool -> Bool
True  || _              =  True
False || x              =  x

-- | Boolean \"not\"
not                     :: Bool -> Bool
not True                =  False
not False               =  True

