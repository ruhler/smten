
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
-- | This module provides the yices1 solver for smten.
module Smten.Symbolic.Solver.Yices1 (yices1) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- | The yices1 smten solver.
yices1 :: Solver
yices1 = primitive "Smten.Symbolic.Solver.Yices1.yices1"

