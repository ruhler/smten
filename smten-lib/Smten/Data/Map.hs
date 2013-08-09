
-- Implementation of binary balanced tree based on Data.Map source from
-- containers-3.0.0.0.

{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Map (
    Map,
    lookup, insert,
    size, empty, singleton, fromList, toList,
  )  where

import Smten.Prelude hiding (lookup, map, filter, null)

data Map k a = Tip | Bin Size k a (Map k a) (Map k a)

type Size = Int

size :: Map k v -> Int
size t = case t of
            Tip -> 0
            Bin sz _ _ _ _ -> sz

lookup :: (Ord k) => k -> Map k v -> Maybe v
lookup k t
  = case t of
      Tip -> Nothing
      Bin _ kx x l r ->
        case compare k kx of
          LT -> lookup k l
          GT -> lookup k r
          EQ -> Just x

empty :: Map k v
empty = Tip

singleton :: k -> v -> Map k v
singleton k v = Bin 1 k v Tip Tip

insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert kx x t =
  case t of
    Tip -> singleton kx x
    Bin sz ky y l r ->
        case compare kx ky of
           LT -> balance ky y (insert kx x l) r
           GT -> balance ky y l (insert kx x r)
           EQ -> Bin sz kx x l r

fromList :: (Ord k) => [(k, v)] -> Map k v
fromList ((k, v):xs) = insert k v (fromList xs)
fromList _ = empty

toList :: Map k v -> [(k, v)]
toList t = toAscList t

toAscList :: Map k a -> [(k, a)]
toAscList t = foldrWithKey (\k x xs -> (k,x):xs) [] t

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey _ z Tip = z
foldrWithKey f z (Bin _ kx x l r) =
    foldrWithKey f (f kx x (foldrWithKey f z r)) l

delta,ratio :: Int
delta = 5
ratio = 2

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sizeL + sizeR <= 1 = Bin sizeX k x l r
  | sizeR >= delta * sizeL = rotateL k x l r
  | sizeL >= delta * sizeR = rotateR k x l r
  | otherwise = Bin sizeX k x l r
  where
    sizeL = size l
    sizeR = size r
    sizeX = sizeL + sizeR + 1

rotateL :: a -> b -> Map a b -> Map a b -> Map a b
rotateL k x l r@(Bin _ _ _ ly ry)
  | size ly < ratio*size ry = singleL k x l r
  | otherwise = doubleL k x l r
rotateL _ _ _ Tip = error "rotateL Tip"

rotateR :: a -> b -> Map a b -> Map a b -> Map a b
rotateR k x l@(Bin _ _ _ ly ry) r
  | size ry < ratio*size ly = singleR k x l r
  | otherwise = doubleR k x l r
rotateR _ _ Tip _ = error "rotateR Tip"

singleL :: a -> b -> Map a b -> Map a b -> Map a b
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3) = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ Tip = error "singleL Tip"

singleR :: a -> b -> Map a b -> Map a b -> Map a b
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3 = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ Tip _ = error "singleR Tip"

doubleL :: a -> b -> Map a b -> Map a b -> Map a b
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"

doubleR :: a -> b -> Map a b -> Map a b -> Map a b
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (size l + size r + 1) k x l r

instance (Show k, Show a) => Show (Map k a) where
    showsPrec d m = showParen (d > 10) $
        showString "fromList " . shows (toList m)
