
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Smten.GHC.Base (
    Functor(..),
    Monad(..),
    (.), const, eqString, foldr, build,
    ) where

import GHC.Types (Bool(..))
import GHC.Classes ((&&), (==))
import Smten.Smten.Base
import Smten.System.IO0

infixr 9 .
infixl 4  <$
infixl 1  >>, >>=

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

const                   :: a -> b -> a
const x _               =  x

{-# INLINE (.) #-}
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)


instance  Functor IO where
   fmap f x = x >>= (return . f)

instance Monad IO where
    return = return_io
    (>>=) = bind_io

eqString :: String -> String -> Bool
eqString [] [] = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString _ _ = False


foldr            :: (a -> b -> b) -> b -> [a] -> b
{-# INLINE [0] foldr #-}
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
build g = g (:) []


