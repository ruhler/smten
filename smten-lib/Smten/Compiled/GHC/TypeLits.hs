
{-# LANGUAGE DataKinds, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Smten.Compiled.GHC.TypeLits (
    SingI, Nat, type (+),
    ) where

import GHC.TypeLits (Nat, type (+))

type family SingI a (b :: Nat)
type instance SingI Nat (b :: Nat) = Integer

