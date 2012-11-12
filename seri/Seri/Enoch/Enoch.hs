
{-# LANGUAGE FlexibleInstances #-}

module Seri.Enoch.Enoch (
    TExp(..),
    SeriableE(..),
    packE, unpackE, unpack',
 ) where

import Data.Maybe (fromMaybe)

import Seri.Lambda
import Seri.Type.Sugar
import Seri.Type.SeriT

-- | Typed Exp.
-- A Seri expression corresponding to a haskell object of type 'a'
data TExp a = TExp Exp

class (SeriT a) => SeriableE a where
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

