
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Symbolic.Solver.Yices2 (yices2) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

yices2 :: Solver
yices2 = primitive "Smten.Symbolic.Solver.Yices2.yices2"

