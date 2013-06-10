
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Runtime.Numeric (
    NumT, (:+:), (:-:), (:*:), Numeric(..),
    Integer
    ) where

import GHC.TypeLits
import qualified Smten.Runtime.SmtenHS as S

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

instance S.SmtenHS0 (NumT n) where
    mux0 _ _ _ = numeric
    realize0 _ _ = numeric
    strict_app0 f x = f x

instance S.SmtenHS2 (:+:) where
    mux2 _ _ _ = numeric
    realize2 _ _ = numeric
    strict_app2 f x = f x

instance S.SmtenHS2 (:-:) where
    mux2 _ _ _ = numeric
    realize2 _ _ = numeric
    strict_app2 f x = f x

instance S.SmtenHS2 (:*:) where
    mux2 _ _ _ = numeric
    realize2 _ _ = numeric
    strict_app2 f x = f x

