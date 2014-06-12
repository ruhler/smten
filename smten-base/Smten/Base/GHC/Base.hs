
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
module Smten.Base.GHC.Base (
    Functor(..),
    Monad(..),
    foldr, build, map, (++),
    otherwise,
    unsafeChr, ord,
    String, eqString,
    minInt, maxInt,
    id, (.), const, flip, ($),
    quotInt, remInt, divInt, modInt,
    quotRemInt, divModInt,
    ) where

import GHC.Prim
import GHC.Types (Char(..), Int(..), Bool(..), IO(..))
import GHC.Classes (divInt#, modInt#, (&&), (==))
import GHC.Err (error)

infixr 9 .
infixr 5 ++
infixl 4 <$
infixl 1 >>, >>=
infixr 0 $

class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const


class Monad m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (>>) :: forall a b. m a -> m b -> m b
    return :: a -> m a
    fail :: String -> m a

    {-# INLINE (>>) #-}
    m >> k = m >>= \_ -> k
    fail s = error s

instance Functor [] where
    fmap = map

instance  Monad []  where
    m >>= k             = foldr ((++) . k) [] m
    m >> k              = foldr ((++) . (\ _ -> k)) [] m
    return x            = [x]
    fail _              = []


foldr            :: (a -> b -> b) -> b -> [a] -> b
{-# INLINE [0] foldr #-}
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys


build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
build g = g (:) []

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

otherwise :: Bool
otherwise = True

type String = [Char]

unsafeChr :: Int -> Char
unsafeChr (I# i#) = C# (chr# i#)

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)

eqString :: String -> String -> Bool
eqString [] [] = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString _ _ = False

maxInt, minInt :: Int
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#


id                      :: a -> a
id x                    =  x

const                   :: a -> b -> a
const x _               =  x

{-# INLINE (.) #-}
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

{-# INLINE ($) #-}
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x


instance  Functor IO where
   fmap f x = x >>= (return . f)

instance Monad IO where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k    = m >>= \ _ -> k
    return    = returnIO
    (>>=)     = bindIO
    --fail s    = failIO s

returnIO :: a -> IO a
returnIO x = IO $ \ s -> (# s, x #)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a


{-# INLINE quotInt #-}
{-# INLINE remInt #-}

quotInt, remInt, divInt, modInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)

quotRemInt :: Int -> Int -> (Int, Int)
(I# x) `quotRemInt` (I# y) = case x `quotRemInt#` y of
                             (# q, r #) ->
                                 (I# q, I# r)

divModInt :: Int -> Int -> (Int, Int)
(I# x) `divModInt` (I# y) = case x `divModInt#` y of
                            (# q, r #) -> (I# q, I# r)

divModInt# :: Int# -> Int# -> (# Int#, Int# #)
x# `divModInt#` y#
 | (x# ># 0#) && (y# <# 0#) = case (x# -# 1#) `quotRemInt#` y# of
                              (# q, r #) -> (# q -# 1#, r +# y# +# 1# #)
 | (x# <# 0#) && (y# ># 0#) = case (x# +# 1#) `quotRemInt#` y# of
                              (# q, r #) -> (# q -# 1#, r +# y# -# 1# #)
 | otherwise                = x# `quotRemInt#` y#

{-# RULES
"x# +# 0#" forall x#. x# +# 0# = x#
"0# +# x#" forall x#. 0# +# x# = x#
"x# -# 0#" forall x#. x# -# 0# = x#
"x# -# x#" forall x#. x# -# x# = 0#
"x# *# 0#" forall x#. x# *# 0# = 0#
"0# *# x#" forall x#. 0# *# x# = 0#
"x# *# 1#" forall x#. x# *# 1# = x#
"1# *# x#" forall x#. 1# *# x# = x#
  #-}

{-# RULES
"x# ># x#"  forall x#. x# >#  x# = False
"x# >=# x#" forall x#. x# >=# x# = True
"x# ==# x#" forall x#. x# ==# x# = True
"x# /=# x#" forall x#. x# /=# x# = False
"x# <# x#"  forall x#. x# <#  x# = False
"x# <=# x#" forall x#. x# <=# x# = True
  #-}

{-# RULES
"plusFloat x 0.0"   forall x#. plusFloat#  x#   0.0# = x#
"plusFloat 0.0 x"   forall x#. plusFloat#  0.0# x#   = x#
"minusFloat x 0.0"  forall x#. minusFloat# x#   0.0# = x#
"timesFloat x 1.0"  forall x#. timesFloat# x#   1.0# = x#
"timesFloat 1.0 x"  forall x#. timesFloat# 1.0# x#   = x#
"divideFloat x 1.0" forall x#. divideFloat# x#  1.0# = x#
  #-}

{-# RULES
"plusDouble x 0.0"   forall x#. (+##) x#    0.0## = x#
"plusDouble 0.0 x"   forall x#. (+##) 0.0## x#    = x#
"minusDouble x 0.0"  forall x#. (-##) x#    0.0## = x#
"timesDouble x 1.0"  forall x#. (*##) x#    1.0## = x#
"timesDouble 1.0 x"  forall x#. (*##) 1.0## x#    = x#
"divideDouble x 1.0" forall x#. (/##) x#    1.0## = x#
  #-}

