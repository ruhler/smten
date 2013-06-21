
{-# LANGUAGE PatternGuards #-}

module Smten.Type.Type (
     Kind(..), Type(..), nteval, ntops,
 ) where

import Smten.Name

type NTOp = String

data Kind = StarK                   -- ^ *
          | NumK                    -- ^ #
          | ArrowK Kind Kind        -- ^ k1 -> k2
          | UnknownK                -- ^ ?
          | VarK Integer
    deriving(Eq, Ord, Show)

data Type = ConT Name Kind                  -- ^ type constructor
          | AppT Type Type                  -- ^ type application
          | VarT Name Kind                  -- ^ type variable
          | NumT Integer                    -- ^ numeric type constructor
          | OpT NTOp Type Type              -- ^ numeric type operation
          | UnknownT
      deriving(Show)

-- Kinds don't affect equality of type
instance Eq Type where
    (==) (ConT a _) (ConT b _) = a == b
    (==) (AppT a1 a2) (AppT b1 b2) = and [a1 == b1, a2 == b2]
    (==) (VarT a _) (VarT b _) = a == b
    (==) (NumT a) (NumT b) = a == b
    (==) (OpT a1 a2 a3) (OpT b1 b2 b3) = and [a1 == b1, a2 == b2, a3 == b3]
    (==) UnknownT UnknownT = True
    (==) _ _ = False

instance Ord Type where
    compare (ConT a _) (ConT b _) = compare a b
    compare (ConT {}) _ = LT
    compare _ (ConT {}) = GT

    compare (AppT a1 a2) (AppT b1 b2) = compare (a1, a2) (b1, b2)
    compare (AppT {}) _ = LT
    compare _ (AppT {}) = GT

    compare (VarT a _) (VarT b _) = compare a b 
    compare (VarT {}) _ = LT
    compare _ (VarT {}) = GT

    compare (NumT a) (NumT b) = compare a b
    compare (NumT {}) _ = LT
    compare _ (NumT {}) = GT

    compare (OpT a1 a2 a3) (OpT b1 b2 b3) = compare (a1, a2, a3) (b1, b2, b3)
    compare (OpT {}) _ = LT
    compare _ (OpT {}) = GT
    
    compare UnknownT UnknownT = EQ
    

-- | Evaluate a concrete numeric type.
nteval :: Type -> Integer
nteval t
  | NumT i <- t = i
  | OpT "+" a b <- t = nteval a + nteval b
  | OpT "-" a b <- t = nteval a - nteval b
  | OpT "*" a b <- t = nteval a * nteval b
  | OpT f a b <- t = error $ "nteval: unknown NTOp: " ++ f
  | VarT {} <- t = error $ "nteval: non-concrete type: " ++ show t
  | otherwise = error $ "nteval: non-numeric type: " ++ show t

ntops :: [NTOp]
ntops = ["+", "-", "*"]

