
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.SymbolicOf (
    SymbolicOf(..), ($$), symapp2,
  ) where

import Smten.Runtime.SmtenHS

infixr 0 $$

class SymbolicOf c s where
    tosym :: c -> s
    symapp :: (SmtenHS0 a) => (c -> a) -> s -> a

($$) :: (SymbolicOf c s, SmtenHS0 a) => (c -> a) -> s -> a
($$) = symapp

symapp2 :: (SymbolicOf c1 s1, SymbolicOf c2 s2, SmtenHS0 a)
            => (c1 -> c2 -> a) -> s1 -> s2 -> a
symapp2 f a b = symapp (\av -> symapp (\bv -> f av bv) b) a

