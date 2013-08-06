
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Array (
    Array, array, listArray, (!), bounds, indices, elems, assocs, (//),
    module Smten.Data.Ix
  ) where

import Smten.Prelude
import Smten.Data.Ix
import Smten.Data.List(sortBy)
import Smten.Data.Array0

data Array i e = MkArray (i, i) (PrimArray e)

array :: forall i e . Ix i => (i, i) -> [(i, e)] -> Array i e
array b ivs =
  let indexed = map (\(x, v) -> (index b x, v)) ivs

      sorted = sortBy (\(x, _) (y, _) -> compare x y) indexed

      mkelems [] [] = []
      mkelems [] _ = error "Array.array: index out of bounds"
      mkelems (x:xs) vs =
        let (me, vs') = span ((==) x . fst) vs
            rest = mkelems xs vs'
            meelem = case me of
                [] -> error "Array.!: undefined array element"
                [(_, v)] -> v
                _ -> error "Array.!: multiply defined array element"
        in meelem : rest

      arr = primArray (mkelems [0..(rangeSize b - 1)] sorted)
  in MkArray b arr

listArray :: Ix i => (i, i) -> [e] -> Array i e
listArray b vs = MkArray b (primArray $ take (rangeSize b) vs)

(!) :: Ix i => Array i e -> i -> e
(!) (MkArray b arr) x = primSelect arr (index b x)

bounds :: Ix i => Array i e -> (i, i)
bounds (MkArray b _) = b

indices :: Ix i => Array i e -> [i]
indices = range . bounds

elems :: Ix i => Array i e -> [e]
elems a = map (\i -> a ! i) (indices a)

assocs :: Ix i => Array i e -> [(i, e)]
assocs a = map (\i -> (i, a!i)) (indices a)

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
(//) a new_ivs =
  let new_is = map fst new_ivs
      old_ivs = map (\i -> (i, a ! i))
                  (filter (\i -> (notElem i new_is)) (indices a))
  in array (bounds a) (old_ivs ++ new_ivs)

instance (Ix i, Eq e) => Eq (Array i e) where
    (==) a b = (assocs a == assocs b)
    (/=) a b = not (a == b)

