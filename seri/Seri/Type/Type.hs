
module Seri.Type.Type (
     NType(..), Type(..), nteval,
 ) where

import Data.Hashable
import Seri.Name

type NTOp = String

-- | Numeric types.
data NType = ConNT Integer   -- ^ numeric type (should be non-negative)
           | VarNT Name      -- ^ numeric type variable
           | AppNT NTOp NType NType -- ^ numeric type operator application
       deriving (Eq, Ord, Show)

instance Hashable NType where
    hash (ConNT i) = hash ("ConNT", i)
    hash (VarNT n) = hash ("VarNT", n)
    hash (AppNT o a b) = hash ("AppNT", o, a, b)

data Type = ConT Name                       -- ^ type constructor
          | AppT Type Type                  -- ^ type application
          | VarT Name                       -- ^ type variable
          | NumT NType                      -- ^ numeric type
          | UnknownT
      deriving(Eq, Ord, Show)

instance Hashable Type where
    hash (ConT n) = hash ("ConT", n)
    hash (AppT a b) = hash ("AppT", a, b)
    hash (VarT n) = hash ("VarT", n)
    hash (NumT n) = hash ("NumT", n)
    hash UnknownT = hash "UnknownT"

-- | Evaluate a concrete numeric type.
nteval :: NType -> Integer
nteval (ConNT i) = i
nteval v@(VarNT {}) = error $ "nteval: non-concrete numeric type: " ++ show v
nteval (AppNT "+" a b) = nteval a + nteval b
nteval (AppNT "-" a b) = nteval a - nteval b
nteval (AppNT "*" a b) = nteval a * nteval b
nteval (AppNT f a b) = error $ "nteval: unknown AppNT op: " ++ f

