
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Smten.Runtime.Numeric (
    S.valueof0, Integer,
    ) where

import GHC.TypeLits
import Smten.Numeric
import qualified Smten.Runtime.SmtenHS as S

instance (SingI n) => S.SmtenHS0 (NumT n) where
    realize0 _ _ = numeric
    cases0 _ = S.concrete numeric
    primitive0 _ _ = numeric
    error0 = error "TODO: error0 for NumT"
    valueof0 x = valueof x

instance S.SmtenHS2 (:+:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :+:"

    valueof2 :: forall a b . (S.SmtenHS0 a, S.SmtenHS0 b) => (a :+: b) -> Integer
    valueof2 _ = S.valueof0 (numeric :: a) + S.valueof0 (numeric :: b)
        

instance S.SmtenHS2 (:-:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :-:"

    valueof2 :: forall a b . (S.SmtenHS0 a, S.SmtenHS0 b) => (a :-: b) -> Integer
    valueof2 _ = S.valueof0 (numeric :: a) - S.valueof0 (numeric :: b)

instance S.SmtenHS2 (:*:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :*:"

    valueof2 :: forall a b . (S.SmtenHS0 a, S.SmtenHS0 b) => (a :*: b) -> Integer
    valueof2 _ = S.valueof0 (numeric :: a) * S.valueof0 (numeric :: b)

