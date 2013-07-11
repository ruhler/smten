
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic0 (
    Symbolic, Solver, run_symbolic,
    return_symbolic, bind_symbolic, 
    mzero_symbolic, mplus_symbolic,
    ) where

import Smten.Prelude
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

data Solver = Solver

-- List implementation of Symbolic monad.
data Symbolic a = Symbolic { s_elems :: [a] }

return_symbolic :: a -> Symbolic a
return_symbolic x = Symbolic [x]

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic (concat $ map s_elems (map f (s_elems x)))

mzero_symbolic :: Symbolic a
mzero_symbolic = Symbolic []

mplus_symbolic :: Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic (s_elems a ++ s_elems b)

run_symbolic :: Solver -> Symbolic a -> IO (Maybe a)
run_symbolic _ (Symbolic []) = return Nothing
run_symbolic _ (Symbolic (x:_)) = return (Just x)

