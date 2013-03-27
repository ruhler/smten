
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
newtype Sum = Sum {
    terms :: Map.Map Product Integer
}

instance Num Sum where
    fromInteger i = mksum (Map.singleton Map.empty i)
    (+) a b = mksum (Map.unionWith (+) (terms a) (terms b))
    (-) a b = a + (negate b)
    (*) = s_times_s
    negate a = mksum (Map.map negate (terms a))
    abs = error "TODO: abs Sum"
    signum = error "TODO: signum Sum"

mksum :: Map.Map Product Integer -> Sum
mksum = Sum . Map.filter (/= 0) . Map.mapKeysWith (+) prune_p

mkpsum :: Product -> Sum
mkpsum p = mksum (Map.singleton p 1)

-- Represent a numeric type as a sum of products.
-- Does not remove unnecessary terms.
sum :: Type -> Maybe Sum
sum t
 | VarT n NumK <- t = Just $ mkpsum (Map.singleton n 1)
 | NumT i <- t = Just $ fromInteger i
 | OpT "+" a b <- t = do
     a' <- sum a
     b' <- sum b
     return $ a' + b'
 | OpT "-" a b <- t = do
     a' <- sum a
     b' <- sum b
     return $ a' - b'
 | OpT "*" a b <- t = do
     a' <- sum a
     b' <- sum b
     return $ a' * b'
 | otherwise = Nothing

p_times_p :: Product -> Product -> Product
p_times_p = Map.unionWith (+)

i_times_s :: Integer -> Sum -> Sum
i_times_s a b = mksum $ Map.map (* a) (terms b)

p_times_s :: Product -> Sum -> Sum
p_times_s a b = mksum $ Map.mapKeys (p_times_p a) (terms b)

s_times_s :: Sum -> Sum -> Sum
s_times_s a b =
  let f :: (Product, Integer) -> Sum
      f (p, i) = i_times_s i (p_times_s p b)
  in mksum $ Map.unionsWith (+) (map (terms . f) (Map.toList (terms a)))

-- Prune away any zero entries
prune_p :: Product -> Product
prune_p = Map.filter (/= 0)

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
  in foldr addNT (NumT 0) (map f $ Map.toList (terms s))

-- Return a canonical form of the given type.
-- In particular, numeric types are put into a canonical form.
canonical :: Type -> Type
canonical t
 | Just s <- sum t = unsum s
 | AppT a b <- t = AppT (canonical a) (canonical b)
 | otherwise = t

