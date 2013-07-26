
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Smten.Compiled.GHC.TypeLits (
    SingI, Nat
    ) where

data Nat

type family SingI a b
type instance SingI Nat b = Integer

