
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic0 (
    Symbolic, Solver, run_symbolic,
    return_symbolic, bind_symbolic, 
    mzero_symbolic, mplus_symbolic,
    free_Integer, free_Bit,
    ) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Smten.TypeLits
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

-- | Primitive type representing a back-end SMT solver.
data Solver

-- | The Symbolic Monad.
--
-- An computation of type \"Symbolic a\" represents a set of possible values
-- of type \"a\".
data Symbolic a = Symbolic { s_elems :: [a] }

-- | The singleton set { x }.
return_symbolic :: a -> Symbolic a
return_symbolic x = Symbolic [x]

-- | Apply the function 'f' to all elements in the set represented by the
-- symbolic computation 'x', and take the union of the resulting sets.
bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic (concat $ map s_elems (map f (s_elems x)))

-- | The empty set { } .
mzero_symbolic :: Symbolic a
mzero_symbolic = Symbolic []

-- | The union of values in a and b.
mplus_symbolic :: Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic (s_elems a ++ s_elems b)

-- | Return Nothing if the given Symbolic computation represents the empty
-- set, otherwise return (Just v) for an arbitrary element of the set.
run_symbolic :: Solver -> Symbolic a -> IO (Maybe a)
run_symbolic _ (Symbolic []) = return Nothing
run_symbolic _ (Symbolic (x:_)) = return (Just x)

-- | The set of all Integers.
free_Integer :: Symbolic Integer
free_Integer = primitive "Smten.Symbolic0.free_Integer"

-- | The set of all bit vectors of size n.
free_Bit :: (SingI n) => Symbolic (Bit n)
free_Bit = primitive "Smten.Symbolic0.free_Bit"

