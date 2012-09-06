
{-# LANGUAGE FlexibleInstances #-}

module Seri.Enoch.Enoch (
    TExp(..),
    SeriableT(..), SeriableT1(..), SeriableT2(..),
    SeriableE(..),
    unpack', varE, conE,
 ) where

import Data.Maybe (fromMaybe)

import Seri.Lambda

-- | Typed Exp.
-- A Seri expression corresponding to a haskell object of type 'a'
data TExp a = TExp Exp

-- | Class of types which have a corresponding seri type.
class SeriableT a where
    -- | The seri type corresponding to the type 'a'.
    -- The argument is ignored.
    serit :: a -> Type

-- | Class of unary type constructors having a corresponding seri type
-- constructor.
class SeriableT1 m where
    -- | The seri unary type constructor corresponding to the type constructor
    -- 'm'. The argument is ignored.
    serit1 :: m a -> Type

instance (SeriableT1 m, SeriableT a) => SeriableT (m a) where
    serit x =
      let t :: m a -> a
          t _ = undefined
      in appsT [serit1 x, serit (t x)]

class SeriableT2 m where
    serit2 :: m a b -> Type

instance (SeriableT2 m, SeriableT a) => SeriableT1 (m a) where
    serit1 x =
      let t :: m a b -> a
          t _ = undefined
      in appsT [serit2 x, serit (t x)]


-- | Class of type of values which can be reperesnted as seri expressions.
class (SeriableT a) => SeriableE a where
    -- | Convert a haskell object to its seri representation.
    pack :: a -> TExp a

    -- | Convert a seri representation to its haskell object
    unpack :: TExp a -> Maybe a


-- | Unpack assuming the seri expression is of the right type.
-- Throws an error if the sery expression is of the wrong type.
unpack' :: (SeriableE a) => TExp a -> a
unpack' = fromMaybe (error "unpack' failed") . unpack

-- | Make a TExp with the given type out of a variable with the given name.
varE :: (SeriableT a) => String -> TExp a
varE nm =
  let t :: TExp a -> a
      t _ = undefined

      me = TExp $ VarE (Sig (name nm) (serit (t me)))
  in me

-- | Make a TExp with the given type out of a constructor with the given name.
conE :: (SeriableT a) => String -> TExp a
conE nm =
  let t :: TExp a -> a
      t _ = undefined

      me = TExp $ ConE (Sig (name nm) (serit (t me)))
  in me

