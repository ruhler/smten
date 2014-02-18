
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Either_ (
    Either(..), either, lefts, rights, partitionEithers,
    ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.Data.Either instead of Smten.Compiled.Smten.Data.Either_

import GHC.Classes
import GHC.Base
import GHC.Show

data Either a b = Left a | Right b
    deriving (Eq, Ord, Show)    -- TODO: derive Read too

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Monad (Either e) where
    return = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

rights   :: [Either a b] -> [b]
rights x = [a | Right a <- x]

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([],[])
 where
  left  a ~(l, r) = (a:l, r)
  right a ~(l, r) = (l, a:r)


