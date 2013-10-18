
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Control.Monad (
    Monad(..), sequence, sequence_, mapM, mapM_,
    MonadPlus(..), msum,
    (=<<), (>=>), (<=<), forever,
    guard, when, unless,
    liftM, liftM2, liftM3, liftM4, liftM5,
    ap,
    ) where

import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Function
import Smten.Data.List0

infixr 1 =<<, <=<, >=>
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

class (Monad m) => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

msum :: MonadPlus m => [m a] -> m a
{-# INLINE msum #-}
msum = foldr mplus mzero

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) f x = x >>= f

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)

forever :: (Monad m) => m a -> m b
forever a = let a' = a >> a' in a'

when :: (Monad m) => Bool -> m () -> m ()
when p s = if p then s else return ()

unless :: (Monad m) => Bool -> m () -> m ()
unless p s = if p then return () else s

liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1 = do 
    x1 <- m1;
    return (f x1)

liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

liftM3 :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3 = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

liftM4 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 f m1 m2 m3 m4 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4 ; return (f x1 x2 x3 x4) }

liftM5 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4 ; x5 <- m5 ; return (f x1 x2 x3 x4 x5) }

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = liftM2 id

