
module Seri.Type.Type (
     NType(..), Type(..), nteval,
 ) where

import Seri.Name

type NTOp = String

-- | Numeric types.
data NType = ConNT Integer   -- ^ numeric type (should be non-negative)
           | VarNT Name      -- ^ numeric type variable
           | AppNT NTOp NType NType -- ^ numeric type operator application
       deriving (Eq, Ord, Show)

data Type = ConT Name                       -- ^ type constructor
          | AppT Type Type                  -- ^ type application
          | VarT Name                       -- ^ type variable
          | NumT NType                      -- ^ numeric type
          | UnknownT
      deriving(Eq, Ord, Show)

-- | Evaluate a concrete numeric type.
nteval :: NType -> Integer
nteval (ConNT i) = i
nteval v@(VarNT {}) = error $ "nteval: non-concrete numeric type: " ++ show v
nteval (AppNT "+" a b) = nteval a + nteval b
nteval (AppNT "-" a b) = nteval a - nteval b
nteval (AppNT "*" a b) = nteval a * nteval b
nteval (AppNT f a b) = error $ "nteval: unknown AppNT op: " ++ f

