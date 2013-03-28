
{-# LANGUAGE PatternGuards #-}

module Smten.Type.Canonical (
   canonical,
   Sum(), tosum, unsum, monomial, linear, divide,
    ) where

import Control.Monad
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

-- given var 'x', produces the sum: x
monomial :: Name -> Sum
monomial n = mkpsum (Map.singleton n 1)

-- Represent a numeric type as a sum of products.
-- Does not remove unnecessary terms.
tosum :: Type -> Maybe Sum
tosum t
 | VarT n NumK <- t = Just $ monomial n
 | NumT i <- t = Just $ fromInteger i
 | OpT "+" a b <- t = do
     a' <- tosum a
     b' <- tosum b
     return $ a' + b'
 | OpT "-" a b <- t = do
     a' <- tosum a
     b' <- tosum b
     return $ a' - b'
 | OpT "*" a b <- t = do
     a' <- tosum a
     b' <- tosum b
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
 | Just s <- tosum t = unsum s
 | AppT a b <- t = AppT (canonical a) (canonical b)
 | otherwise = t

-- Test if the variable with given name is linear in the given sum. If so,
-- returns its coefficient, otherwise returns nothing.
linear :: Name -> Sum -> Maybe Integer
linear n s = do
    guard $ length (filter (Map.member n) (Map.keys (terms s))) == 1
    Map.lookup (Map.singleton n 1) (terms s)

-- Divide the sum by a value.
-- Succeeds only if all coefficients are evently divisible by that value.
divide :: Sum -> Integer -> Maybe Sum
divide s x = do
  let dividable :: Integer -> Bool
      dividable i = i `rem` x == 0

  guard $ all dividable (Map.elems (terms s))
  return $ mksum (Map.map (\i -> i `div` x) (terms s))

