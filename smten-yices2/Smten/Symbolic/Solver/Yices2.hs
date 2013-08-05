
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

-- | This module provides a Yices2 backend for smten.
module Smten.Symbolic.Solver.Yices2 (yices2) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- | The Yices2 Solver
yices2 :: Solver
yices2 = primitive "Smten.Symbolic.Solver.Yices2.yices2"

