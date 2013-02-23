
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
          | VarK Name
    deriving(Eq, Ord, Show)

data Type = ConT Name                       -- ^ type constructor
          | AppT Type Type                  -- ^ type application
          | VarT Name Kind                  -- ^ type variable
          | NumT Integer                    -- ^ numeric type constructor
          | OpT NTOp Type Type              -- ^ numeric type operation
          | UnknownT
      deriving(Eq, Ord, Show)

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

