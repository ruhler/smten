
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Smten.Smten.TypeLits (
    Nat, SingI, type (+),
 ) where

import qualified Prelude as P
import GHC.TypeLits

type instance 2 + 3 = 5
type instance 3 + 2 = 5

