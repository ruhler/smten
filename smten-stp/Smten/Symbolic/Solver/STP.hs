
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic.Solver.STP (stp) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

stp :: Solver
stp = primitive "Smten.Symbolic.Solver.STP.stp"

