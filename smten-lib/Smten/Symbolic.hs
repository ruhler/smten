
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic (
    Symbolic, Solver, run_symbolic,
    MonadPlus(..),
    free_Bool, assert,
    ) where

import Smten.Prelude
import Smten.Symbolic0
import Smten.Control.Monad

instance Monad Symbolic where
    return = return_symbolic
    (>>=) = bind_symbolic
    fail _ = mzero

instance MonadPlus Symbolic where
    mzero = mzero_symbolic
    mplus = mplus_symbolic

free_Bool :: Symbolic Bool
free_Bool = mplus (return True) (return False)

assert :: Bool -> Symbolic ()
assert p = if p then return () else mzero

