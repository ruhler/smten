
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Search.Prim (
    Space, Solver, search,
    empty, single, union, bind, 
    free_Integer, free_Bit,
    ) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Smten.TypeLits
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

-- | Primitive type representing a back-end SMT solver.
data Solver

-- | The Space Monad.
--
-- An computation of type \"Space a\" represents a set of possible values
-- of type \"a\".
data Space a

-- | The singleton set { x }.
{-# NOINLINE single #-}
single :: a -> Space a
single = primitive "Smten.Search.Prim.single"

-- | Apply the function 'f' to all elements in the set represented by the
-- symbolic computation 'x', and take the union of the resulting sets.
{-# NOINLINE bind #-}
bind :: Space a -> (a -> Space b) -> Space b
bind = primitive "Smten.Search.Prim.bind"

-- | The empty set { } .
{-# NOINLINE empty #-}
empty :: Space a
empty = primitive "Smten.Search.Prim.empty"

-- | The union of values in a and b.
{-# NOINLINE union #-}
union :: Space a -> Space a -> Space a
union = primitive "Smten.Search.Prim.union"

-- | Return Nothing if the given Space computation represents the empty
-- set, otherwise return (Just v) for an arbitrary element of the set.
{-# NOINLINE search #-}
search :: Solver -> Space a -> IO (Maybe a)
search = primitive "Smten.Search.Prim.search"

-- | The set of all Integers.
{-# NOINLINE free_Integer #-}
free_Integer :: Space Integer
free_Integer = primitive "Smten.Search.Prim.free_Integer"

-- | The set of all bit vectors of size n.
{-# NOINLINE free_Bit #-}
free_Bit :: (SingI n) => Space (Bit n)
free_Bit = primitive "Smten.Search.Prim.free_Bit"

