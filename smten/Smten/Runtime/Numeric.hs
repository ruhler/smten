
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
    realize0 _ _ = numeric
    cases0 _ = S.concrete numeric
    primitive0 _ _ = numeric

instance S.SmtenHS2 (:+:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric

instance S.SmtenHS2 (:-:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric

instance S.SmtenHS2 (:*:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric

