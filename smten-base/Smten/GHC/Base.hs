
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

module Smten.GHC.Base (
    Functor(..),
    Monad(..),
    foldr, build, map, (++),
    otherwise,
    unsafeChr, ord,
    String, eqString,
    id, (.), const, flip, ($),
    ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Base instead of Smten.Compiled.Smten.GHC.Base

import GHC.Prim (RealWorld, State#, ord#, chr#)
import GHC.Types (Char(..), Int(..), Bool(..), IO(..))
import GHC.Classes ((&&), (==))
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

