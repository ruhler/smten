
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.NumT (
    Numeric(..), NumT, (:+:), (:-:), (:*:),
    numeric,
  ) where

import GHC.TypeLits

data NumT (n :: Nat)
data a :+: b
data a :-: b
data a :*: b

numeric :: a
numeric = error "numeric"

class Numeric a where
    valueof :: a -> Integer

instance (SingI n) => Numeric (NumT n) where
    valueof x =
      let f :: NumT n -> Sing n -> Integer
          f _ n = fromSing n
      in withSing (f x)

instance (Numeric a, Numeric b) => Numeric (a :+: b) where
    valueof _ = valueof (numeric :: a) + valueof (numeric :: b)

instance (Numeric a, Numeric b) => Numeric (a :-: b) where
    valueof _ = valueof (numeric :: a) - valueof (numeric :: b)

instance (Numeric a, Numeric b) => Numeric (a :*: b) where
    valueof _ = valueof (numeric :: a) * valueof (numeric :: b)

