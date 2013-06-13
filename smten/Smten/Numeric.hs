
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Numeric (
    NumT, (:+:), (:-:), (:*:), Numeric(..), numeric,
    ) where

import GHC.TypeLits

data NumT (n :: Nat)
data a :+: b
data a :-: b
data a :*: b

class Numeric a where   
    valueof :: a -> Integer

numeric :: a
numeric = error "numeric"
    
instance (SingI n) => Numeric (NumT n) where
    valueof x =
      let f :: NumT n -> Sing n -> Integer
          f _ n = fromSing n
      in withSing (f x)

instance (Numeric a, Numeric b) => Numeric (a :+: b) where
    valueof x = valueof (numeric :: a) + valueof (numeric :: b)

instance (Numeric a, Numeric b) => Numeric (a :-: b) where
    valueof x = valueof (numeric :: a) - valueof (numeric :: b)

instance (Numeric a, Numeric b) => Numeric (a :*: b) where
    valueof x = valueof (numeric :: a) * valueof (numeric :: b)

