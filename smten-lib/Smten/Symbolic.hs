
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides the Symbolic monad, for orchestration of symbolic
-- computations.
module Smten.Symbolic (
    Symbolic, Solver, run_symbolic,
    MonadPlus(..),
    free_Bool, free_Integer, free_Bit, assert,
    Free(..),
    ) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Smten.TypeLits
import Smten.Symbolic0
import Smten.Control.Monad

instance Functor Symbolic where
    fmap f x = do
        v <- x
        return (f v)

instance Monad Symbolic where
    return = return_symbolic
    (>>=) = bind_symbolic
    fail _ = mzero

instance MonadPlus Symbolic where
    mzero = mzero_symbolic
    mplus = mplus_symbolic

-- | The set of booleans { True, False }
free_Bool :: Symbolic Bool
free_Bool = mplus (return True) (return False)

-- | Assert the given predicate is satisfied.
assert :: Bool -> Symbolic ()
assert p = if p then return () else mzero

class Free a where
    free :: Symbolic a

instance Free Bool where
    free = free_Bool

instance Free Integer where
    free = free_Integer

instance (KnownNat n) => Free (Bit n) where
    free = free_Bit

instance (Free a) => Free (Maybe a) where
    free = do
       isJust <- free
       v <- free
       return (if isJust then Just v else Nothing)

