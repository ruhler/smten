
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides the Space monad.
-- An alternate presentation of the Smten API for serach
module Smten.Search (
  Space, Solver, search,
  empty, single, union,
  MonadPlus(..), guard,
  Free(..), free_Bool, free_Integer, free_Bit,
  ) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Smten.TypeLits
import Smten.Control.Monad
import Smten.Search.Prim

type Space = Symbolic

empty :: Space a
empty = mzero 

single :: a -> Space a
single = return

union :: Space a -> Space a -> Space a
union = mplus

search :: Solver -> Space a -> IO (Maybe a)
search = run_symbolic

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
free_Bool :: Space Bool
free_Bool = mplus (return True) (return False)

class Free a where
    free :: Space a

instance Free Bool where
    free = free_Bool

instance Free Integer where
    free = free_Integer

instance (SingI n) => Free (Bit n) where
    free = free_Bit

instance (Free a) => Free (Maybe a) where
    free = do
       isJust <- free
       v <- free
       return (if isJust then Just v else Nothing)

