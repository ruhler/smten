
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Symbolic.Solver.Pure (pure) where

import Smten.Symbolic0
import Smten.Search.Solver.Smten

{-# DEPRECATED pure "Please use the 'smten' solver instead" #-}
pure :: Solver
pure = smten

