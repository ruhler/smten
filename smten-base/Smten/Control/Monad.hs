
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Control.Monad (
    Monad(..), sequence, sequence_, mapM, mapM_,
    MonadPlus(..), msum,
    ) where

import Smten.Smten.Base
import Smten.Data.List0

infixl 1 >>, >>=

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    (>>) x y = x >>= \_ -> y
    fail :: String -> m a
    fail s = error s

sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])
   where mcons p q = p >>= \x -> q >>= \y -> return (x:y)

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence (map f as)

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence_ (map f as)

class MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

msum :: MonadPlus m => [m a] -> m a
{-# INLINE msum #-}
msum = foldr mplus mzero

