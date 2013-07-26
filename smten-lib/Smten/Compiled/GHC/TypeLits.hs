
{-# LANGUAGE DataKinds, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Smten.Compiled.GHC.TypeLits (
    SingI, Nat,
    ) where

import GHC.TypeLits (Nat)

type family SingI a (b :: Nat)
type instance SingI Nat (b :: Nat) = Integer

