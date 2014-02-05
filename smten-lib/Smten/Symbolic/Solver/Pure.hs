
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Symbolic.Solver.Pure (pure) where

import Smten.Symbolic
import Smten.Symbolic.Solver.Smten

{-# DEPRECATED pure "Please use the 'smten' solver instead" #-}
pure :: Solver
pure = smten

