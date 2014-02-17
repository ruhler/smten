
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Smten.GHC.Base (
    Functor(..),
    Monad(..),
    (.), const,
    ) where

import Smten.Smten.Base
import Smten.System.IO0

infixr 9 .
infixl 4  <$

class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const


-- Note: this definition must match up with the definition from
-- GHC.Base
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
