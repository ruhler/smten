
{-# LANGUAGE FlexibleInstances #-}

-- | Relating haskell types to seri types.
module Seri.Type.SeriT (
    SeriT(..), SeriT1(..), SeriT2(..), SeriT3(..), SeriT4(..),
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
    seriT1 :: (SeriT a) => m a -> Type

class SeriT2 m where
    seriT2 :: (SeriT a, SeriT b) => m a b -> Type

class SeriT3 m where
    seriT3 :: (SeriT a, SeriT b, SeriT c) => m a b c -> Type

class SeriT4 m where
    seriT4 :: (SeriT a, SeriT b, SeriT c, SeriT d) => m a b c d -> Type

instance (SeriT1 m, SeriT a) => SeriT (m a) where
    seriT x =
      let t :: m a -> a
          t _ = undefined
      in appT (seriT1 x) (seriT (t x))


instance (SeriT2 m, SeriT a) => SeriT1 (m a) where
    seriT1 x =
      let t :: m a b -> a
          t _ = undefined
      in appT (seriT2 x) (seriT (t x))

instance (SeriT3 m, SeriT a) => SeriT2 (m a) where
    seriT2 x =
      let t :: m a b c -> a
          t _ = undefined
      in appT (seriT3 x) (seriT (t x))

instance (SeriT4 m, SeriT a) => SeriT3 (m a) where
    seriT3 x =
      let t :: m a b c d -> a
          t _ = undefined
      in appT (seriT4 x) (seriT (t x))


instance SeriT () where
    seriT _ = unitT

instance SeriT Char where
    seriT _ = charT

instance SeriT Integer where
    seriT _ = integerT

instance SeriT Bool where
    seriT _ = boolT

instance SeriT1 IO where
    seriT1 _ = conT (name "IO")

instance SeriT2 (->) where
    seriT2 _ = conT arrowN

instance SeriT2 (,) where
    seriT2 _ = conT (name "(,)")

