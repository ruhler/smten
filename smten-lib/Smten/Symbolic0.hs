
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Symbolic0 (
    Symbolic, Solver, run_symbolic,
    return_symbolic, bind_symbolic, 
    mzero_symbolic, mplus_symbolic,
    free_Integer, free_Bit,
    ) where

import Smten.Prelude
import Smten.Data.Bit
import GHC.TypeLits
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

-- | Primitive type representing a back-end SMT solver.
data Solver

-- | The Symbolic Monad.
--
-- An computation of type \"Symbolic a\" represents a set of possible values
-- of type \"a\".
data Symbolic a

-- | The singleton set { x }.
{-# NOINLINE return_symbolic #-}
return_symbolic :: a -> Symbolic a
return_symbolic = primitive "Smten.Symbolic0.return_symbolic"

-- | Apply the function 'f' to all elements in the set represented by the
-- symbolic computation 'x', and take the union of the resulting sets.
{-# NOINLINE bind_symbolic #-}
bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = primitive "Smten.Symbolic0.bind_symbolic"

-- | The empty set { } .
{-# NOINLINE mzero_symbolic #-}
mzero_symbolic :: Symbolic a
mzero_symbolic = primitive "Smten.Symbolic0.mzero_symbolic"

-- | The union of values in a and b.
{-# NOINLINE mplus_symbolic #-}
mplus_symbolic :: Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic = primitive "Smten.Symbolic0.mplus_symbolic"

-- | Return Nothing if the given Symbolic computation represents the empty
-- set, otherwise return (Just v) for an arbitrary element of the set.
{-# NOINLINE run_symbolic #-}
run_symbolic :: Solver -> Symbolic a -> IO (Maybe a)
run_symbolic = primitive "Smten.Symbolic0.run_symbolic"

-- | The set of all Integers.
{-# NOINLINE free_Integer #-}
free_Integer :: Symbolic Integer
free_Integer = primitive "Smten.Symbolic0.free_Integer"

-- | The set of all bit vectors of size n.
{-# NOINLINE free_Bit #-}
free_Bit :: (KnownNat n) => Symbolic (Bit n)
free_Bit = primitive "Smten.Symbolic0.free_Bit"

