
{-# LANGUAGE FlexibleInstances #-}

module Seri.Type.SeriT (
    SeriT(..), SeriT1(..), SeriT2(..),
 ) where

import Seri.Name
import Seri.Type.Type
import Seri.Type.Sugar
   
-- | Class of haskell types which have a corresponding seri type.
class SeriT a where
    -- | The seri type corresponding to the type 'a'.
    -- The argument is ignored.
    seriT :: a -> Type

-- | Class of unary type constructors having a corresponding seri type
-- constructor.
class SeriT1 m where
    -- | The seri unary type constructor corresponding to the type constructor
    -- 'm'. The argument is ignored.
    seriT1 :: m a -> Type

instance (SeriT1 m, SeriT a) => SeriT (m a) where
    seriT x =
      let t :: m a -> a
          t _ = undefined
      in appT (seriT1 x) (seriT (t x))

class SeriT2 m where
    seriT2 :: m a b -> Type

instance (SeriT2 m, SeriT a) => SeriT1 (m a) where
    seriT1 x =
      let t :: m a b -> a
          t _ = undefined
      in appT (seriT2 x) (seriT (t x))

instance SeriT () where
    seriT _ = conT (name "()")

instance SeriT Char where
    seriT _ = conT (name "Char")

instance SeriT Integer where
    seriT _ = conT (name "Integer")

instance SeriT Bool where
    seriT _ = conT (name "Bool")

instance SeriT2 (->) where
    seriT2 _ = conT arrowN

instance SeriT2 (,) where
    seriT2 _ = conT (name "(,)")


