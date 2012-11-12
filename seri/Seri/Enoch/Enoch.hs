
{-# LANGUAGE FlexibleInstances #-}

module Seri.Enoch.Enoch (
    TExp(..),
    SeriableT(..), SeriableT1(..), SeriableT2(..),
    SeriableE(..),
    packE, unpackE, unpack',
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

packE :: (SeriableE a) => a -> Exp
packE a = 
 let TExp e = pack a
 in e

unpackE :: (SeriableE a) => Exp -> Maybe a
unpackE e = unpack (TExp e)

-- | Unpack assuming the seri expression is of the right type.
-- Throws an error if the sery expression is of the wrong type.
unpack' :: (SeriableE a) => TExp a -> a
unpack' t@(TExp x) =
 case unpack t of
    Just v -> v
    Nothing -> error $ "unpack' failed on " ++ pretty x

