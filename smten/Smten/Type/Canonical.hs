
{-# LANGUAGE PatternGuards #-}

module Smten.Type.Canonical (canonical) where

import Prelude hiding (sum)
import qualified Data.Map as Map

import Data.Functor ((<$>))
import Data.List (genericReplicate)

import Smten.Name
import Smten.Type.Type

-- A Product:
--  For each variable in the term, gives the power of that variable.
type Product = Map.Map Name Integer

-- Gives the coefficient of each product.
type Sum = Map.Map Product Integer

-- Represent a numeric type as a sum of products.
-- Does not remove unnecessary terms.
sum :: Type -> Maybe Sum
sum t
 | VarT n NumK <- t = Just $ Map.singleton (Map.singleton n 1) 1
 | NumT i <- t = Just $ Map.singleton Map.empty i
 | OpT "+" a b <- t = do
     a' <- sum a
     b' <- sum b
     return $ Map.unionWith (+) a' b'
 | OpT "-" a b <- t = do
     a' <- sum a
     b' <- Map.map negate <$> sum b
     return $ Map.unionWith (+) a' b'
 | OpT "*" a b <- t = do
     a' <- sum a
     b' <- sum b
     return $ s_times_s a' b'
 | otherwise = Nothing

p_times_p :: Product -> Product -> Product
p_times_p = Map.unionWith (+)

i_times_s :: Integer -> Sum -> Sum
i_times_s a b = Map.map (* a) b

p_times_s :: Product -> Sum -> Sum
p_times_s a = Map.mapKeys (p_times_p a)

s_times_s :: Sum -> Sum -> Sum
s_times_s a b =
  let f :: (Product, Integer) -> Sum
      f (p, i) = i_times_s i (p_times_s p b)
  in Map.unionsWith (+) (map f (Map.toList a))

-- Prune away any zero entries
prune_p :: Product -> Product
prune_p = Map.filter (/= 0)

prune_s :: Sum -> Sum
prune_s = Map.filter (/= 0) . Map.mapKeysWith (+) prune_p

addNT :: Type -> Type -> Type
addNT a b
  | NumT 0 <- a = b
  | NumT 0 <- b = a
  | otherwise = OpT "+" a b

mulNT :: Type -> Type -> Type
mulNT a b
  | NumT 0 <- a = a
  | NumT 0 <- b = b
  | NumT 1 <- a = b
  | NumT 1 <- b = a
  | otherwise = OpT "*" a b

unproduct :: Product -> Type
unproduct p =
  let f :: (Name, Integer) -> [Type]
      f (n, i) = genericReplicate i (VarT n NumK)
  in foldr mulNT (NumT 1) (concatMap f $ Map.toList p)

unsum :: Sum -> Type
unsum s = 
  let f :: (Product, Integer) -> Type
      f (p, i) | i < 0 = OpT "-" (NumT 0) (f (p, negate i))
      f (p, i) = mulNT (NumT i) (unproduct p)
  in foldr addNT (NumT 0) (map f $ Map.toList s)

-- Return a canonical form of the given type.
-- In particular, numeric types are put into a canonical form.
canonical :: Type -> Type
canonical t
 | Just s <- sum t = unsum $ prune_s s
 | AppT a b <- t = AppT (canonical a) (canonical b)
 | otherwise = t

