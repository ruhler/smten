
{-# LANGUAGE DataKinds, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Smten.Compiled.GHC.TypeLits (
    SingI, DGSingI(..), Nat, type (+),
    ) where

import GHC.TypeLits (Nat, type (+))

type family SingI a (b :: Nat)
type instance SingI Nat (b :: Nat) = DGSingI

newtype DGSingI = DGSingI {
    __deNewTyDGSingI :: Integer
}

instance Num DGSingI where
    fromInteger = DGSingI . fromInteger
    (+) = error "No (+) for SingI"
    (*) = error "No (*) for SingI"
    abs = error "No abs for SingI"
    signum = error "No signum for SingI"

