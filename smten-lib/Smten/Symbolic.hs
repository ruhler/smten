
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic (
    Symbolic, Solver, run_symbolic,
    MonadPlus(..),
    free_Bool, assert,
    ) where

import Smten.Prelude
import Smten.Control.Monad

data Solver = Solver

-- List implementation of Symbolic monad.
data Symbolic a = Symbolic { s_elems :: [a] }

instance Monad Symbolic where
    return x = Symbolic [x]
    (>>=) x f = Symbolic (concat $ map s_elems (map f (s_elems x)))
    fail _ = mzero

instance MonadPlus Symbolic where
    mzero = Symbolic []
    mplus a b = Symbolic (s_elems a ++ s_elems b)

run_symbolic :: Solver -> Symbolic a -> IO (Maybe a)
run_symbolic _ (Symbolic []) = return Nothing
run_symbolic _ (Symbolic (x:_)) = return (Just x)

free_Bool :: Symbolic Bool
free_Bool = mplus (return True) (return False)

assert :: Bool -> Symbolic ()
assert p = if p then return () else mzero

