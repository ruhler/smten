
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a Yices2 backend for smten.
module Smten.Symbolic.Solver.Yices2
  {-# DEPRECATED "Use Smten.Search.Solver.Yices2 instead" #-}
 (yices2) where

import Smten.Plugin.Annotations
import Smten.Prelude
import Smten.Symbolic

{-# ANN module PrimitiveModule #-}

-- | The Yices2 Solver
--
-- The yices2 backend requires the yices2 shared library is installed at
-- runtime. Yices2 can be downloaded from 
-- <http://yices.csl.sri.com/download-yices2.shtml>. The yices2 backend
-- searches for libyices.so.2.1 to be installed in standard library paths.
--
-- You may need to provide a symlink for libyices.so.2.1, either by running
-- ldconfig or manually pointing to your version of yices2. You may also need
-- to add the directory containing libyices.so.2.1 to the LD_LIBRARY_PATH
-- environment variable at runtime.
yices2 :: Solver
yices2 = primitive "Smten.Symbolic.Solver.Yices2.yices2"

