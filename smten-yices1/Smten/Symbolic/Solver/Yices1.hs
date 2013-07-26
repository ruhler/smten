
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic.Solver.Yices1 (yices1) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

yices1 :: Solver
yices1 = primitive "Smten.Symbolic.Solver.Yices1.yices1"

