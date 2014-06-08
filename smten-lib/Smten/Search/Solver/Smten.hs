
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Search.Solver.Smten (smten) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Search.Prim

{-# ANN module PrimitiveModule #-}

-- | A smten backend developed entirely in Haskell.
-- This is intended to make it easier to try symbolic computations without
-- having to install a special backend, and also just for the fun of it.
smten :: Solver
smten = error "Smten.Search.Solver.Smten.smten"

