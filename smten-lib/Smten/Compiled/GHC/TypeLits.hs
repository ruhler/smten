
{-# LANGUAGE DataKinds, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Smten.Compiled.GHC.TypeLits (
    KnownNat(..), SNat(..), knownNatVal, Nat, type (+),
    ) where

import GHC.TypeLits (Nat, type (+))

import Smten.Runtime.Formula.IntegerF
import Smten.Runtime.Formula.Finite

newtype SNat (x :: Nat) = SNat IntegerF
newtype KnownNat (x :: Nat) = DGKnownNat (SNat x)

knownNatVal :: KnownNat x -> Integer
knownNatVal (DGKnownNat (SNat x)) =
  case deIntegerF x of
     (TrueFF, IntegerFF v, _) -> v

instance Num (KnownNat n) where
    fromInteger = DGKnownNat . SNat . fromInteger
    (+) = error "No (+) for KnownNat"
    (-) = error "No (-) for KnownNat"
    (*) = error "No (*) for KnownNat"
    abs = error "No abs for KnownNat"
    signum = error "No signum for KnownNat"

