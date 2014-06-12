
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -O #-}
module Smten.Base.GHC.Classes (
    (&&), (||), not,
    Eq(..), Ord(..),
    divInt#, modInt#,
    ) where

import qualified GHC.Classes as P
import GHC.Prim
import GHC.Types
import Smten.Smten.Unit
import Smten.Smten.List
import Smten.Smten.Tuple

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

deriving instance P.Eq Unit__
deriving instance (P.Eq a, P.Eq b) => P.Eq (Tuple2__ a b)
deriving instance (P.Eq a, P.Eq b, P.Eq c) => P.Eq (Tuple3__ a b c)
deriving instance (P.Eq a, P.Eq b, P.Eq c, P.Eq d) => P.Eq (Tuple4__ a b c d)
deriving instance (P.Eq a) => P.Eq (List__ a)

-- TODO: can we auto-derive this?
instance Eq Bool where
    (==) True True = True
    (==) True False = False
    (==) False True = False
    (==) False False = True

instance Eq Char where
    (C# c1) == (C# c2) = c1 `eqChar#` c2
    (C# c1) /= (C# c2) = c1 `neChar#` c2

-- TODO: can we auto-derive this?
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

deriving instance P.Ord Unit__
deriving instance (P.Ord a, P.Ord b) => P.Ord (Tuple2__ a b)
deriving instance (P.Ord a, P.Ord b, P.Ord c) => P.Ord (Tuple3__ a b c)
deriving instance (P.Ord a, P.Ord b, P.Ord c, P.Ord d) => P.Ord (Tuple4__ a b c d)
deriving instance (P.Ord a) => P.Ord (List__ a)

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


divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    =      if (x# ># 0#) && (y# <# 0#) then ((x# -# 1#) `quotInt#` y#) -# 1#
      else if (x# <# 0#) && (y# ># 0#) then ((x# +# 1#) `quotInt#` y#) -# 1#
      else x# `quotInt#` y#

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    = if (x# ># 0#) && (y# <# 0#) ||
         (x# <# 0#) && (y# ># 0#)
      then if r# /=# 0# then r# +# y# else 0#
      else r#
    where
    !r# = x# `remInt#` y#
