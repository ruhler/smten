
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Smten.Runtime.Numeric (
    valueof0, Integer,
    NumT, (:+:), (:-:), (:*:),
    ) where

import GHC.TypeLits
import Smten.Runtime.SmtenHS hiding (Integer)

data NumT (n :: Nat)
data a :+: b
data a :-: b
data a :*: b

instance (SingI n) => SmtenHS0 (NumT n) where
    realize0 _ _ = numeric
    primitive0 _ _ = numeric
    ite0 = numeric
    sapp0 = ($)
    error0 = error "TODO: error0 for NumT"
    valueof0 x =
      let f :: NumT n -> Sing n -> Integer
          f _ n = fromSing n
      in withSing (f x)

instance SmtenHS2 (:+:) where
    realize2 _ _ = numeric
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :+:"
    ite2 = numeric
    sapp2 = ($)

    valueof2 :: forall a b . (SmtenHS0 a, SmtenHS0 b) => (a :+: b) -> Integer
    valueof2 _ = valueof0 (numeric :: a) + valueof0 (numeric :: b)
        

instance SmtenHS2 (:-:) where
    realize2 _ _ = numeric
    primitive2 _ _ = numeric
    ite2 = numeric
    sapp2 = ($)
    error2 = error "TODO: error0 for :-:"

    valueof2 :: forall a b . (SmtenHS0 a, SmtenHS0 b) => (a :-: b) -> Integer
    valueof2 _ = valueof0 (numeric :: a) - valueof0 (numeric :: b)

instance SmtenHS2 (:*:) where
    realize2 _ _ = numeric
    ite2 _ _ _ = numeric
    sapp2 = ($)
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :*:"

    valueof2 :: forall a b . (SmtenHS0 a, SmtenHS0 b) => (a :*: b) -> Integer
    valueof2 _ = valueof0 (numeric :: a) * valueof0 (numeric :: b)

