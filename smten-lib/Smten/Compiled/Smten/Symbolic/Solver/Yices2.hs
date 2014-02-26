
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Implementation of the Yices2 backend for smten.
module Smten.Compiled.Smten.Symbolic.Solver.Yices2 (yices2) where

import Foreign

import Smten.Runtime.Yices2.FFI
import Smten.Runtime.Yices2.AST
import Smten.Runtime.Solver

yices2 :: Solver
yices2 = solverFromAST $ do
  r <- c_yices_load
  case r of
    0 -> return ()
    _ -> error "yices2 smten backend: unable to load libyices.so.2.1"
  c_yices_init
  ptr <- c_yices_new_context nullPtr
  return $ Yices2 ptr

