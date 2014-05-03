
{-# LANGUAGE DataKinds, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Smten.Compiled.GHC.TypeLits (
    KnownNat, DGKnownNat(..), knownNatVal, Nat, type (+),
    ) where

import GHC.TypeLits (Nat, type (+))

import Smten.Runtime.Formula.IntegerF
import Smten.Runtime.Formula.Finite

type family KnownNat (b :: Nat)
type instance KnownNat (b :: Nat) = DGKnownNat

newtype DGKnownNat = DGKnownNat IntegerF

knownNatVal :: DGKnownNat -> Integer
knownNatVal (DGKnownNat x) =
  case deIntegerF x of
     (TrueFF, IntegerFF v, _) -> v

instance Num DGKnownNat where
    fromInteger = DGKnownNat . fromInteger
    (+) = error "No (+) for KnownNat"
    (-) = error "No (-) for KnownNat"
    (*) = error "No (*) for KnownNat"
    abs = error "No abs for KnownNat"
    signum = error "No signum for KnownNat"

