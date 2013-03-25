
module Smten.Type.Canonical (canonical) where

import qualified Data.Map as Map

import Data.List (genericReplicate)
import Data.Maybe (fromMaybe)

-- A Product:
--  For each variable in the term, gives the power of that variable.
type Product = Map.Map Name Integer

-- Gives the coefficient of each product.
type Sum = Map.Map Product Integer

-- Represent a numeric type as a sum of products.
-- Does not remove unnecessary terms.
sum :: Type -> Maybe Sum
sum t
 | VarT n _ <- t = Map.singleton (Map.singleton n 1) 1
 | NumT i <- t = Map.singleton Map.empty i
 | OpT "+" a b = Map.unionWith (+) (sum a) (sum b)
 | OpT "-" a b = Map.unionWith (+) (sum a) (Map.map negate (sum b))
 | OpT "*" a b =
 | otherwise = Nothing

p_times_p :: Product -> Product -> Product
p_times_p a b = Map.unionWith (+)

i_times_s :: Integer -> Sum -> Sum
i_times_s a b = Map.map (* a) b

p_times_s :: Product -> Sum -> Sum
p_times_s a = Map.mapKeys (p_times_p a)

s_times_s :: Sum -> Sum -> Sum
s_times_s a b =
  let f :: (Product, Integer) -> Sum
      f (p, i) = i_times_s (p_times_s p b)
  in Map.unionsWith (+) (map f (Map.toList a))

-- Prune away any zero entries
prune_p :: Product -> Product
prune_p = Map.filter (/= 0)

prune_s :: Sum -> Sum
prune_s = Map.filter (/= 0) . Map.mapKeysWith (+) prune_p

unproduct :: Product -> Type
unproduct p =
  let f :: (Name, Integer) -> [Type]
      f (n, i) = genericReplicate i (VarT n NumK)
  in foldr (OpT "*") (NumT 1) (concatMap f $ Map.toList p)

unsum :: Sum -> Type
unsum s = 
  let f :: (Product, Integer) -> Type
      f (p, i) | i < 0 = OpT "-" (NumT 0) (f (p, negate i))
      f (p, i) = OpT "*" (NumT i) (unproduct p)
  in foldr (OpT "+") (NumT 0) (map f $ Map.toList s)

-- Return a canonical form of the given type.
-- In particular, numeric types are put into a canonical form.
canonical :: Type -> Type
canonical t
 | Just s <- sum t = unsum $ prune_s s
 | AppT a b = AppT (canonical a) (canonical b)
 | otherwise = t

